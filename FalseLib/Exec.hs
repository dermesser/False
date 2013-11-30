module FalseLib.Exec where

import FalseLib.Parse

import qualified Data.Map.Strict as Map
import Data.Bits
import Data.List.Split
import Text.Printf
import System.IO
import Control.Monad.Error
import Control.Monad.State.Strict

-- types

data FalseData = FalseNum Int
               | FalseChar Char
               | FalseLambda [FCodePos]
               | FalseVRef Char
               deriving Show

type FStack = [FalseData]
type FMap = Map.Map Char FalseData

type FState = (FStack,FMap)

-- FExecM a => FState -> IO (Either String (a,FState))
type FExecM = StateT FState (ErrorT String IO)
-- Utilities

fTrue :: Int
fTrue = complement 0

emptyStack :: FStack
emptyStack = []

emptyMap :: FMap
emptyMap = Map.empty

emptyState :: FState
emptyState = (emptyStack,emptyMap)

fPush :: FalseData -> FExecM ()
fPush d = StateT $ \(s,m) -> return ((),(d:s,m))

fPop :: Int -> FExecM FalseData
fPop p = StateT f
    where f :: FState -> ErrorT String (IO) (FalseData,FState)
          f ([],m) = throwError ("Runtime error :: Empty stack at " ++ show p ++ "!")
          f (s:ss,m) = return (s,(ss,m))

fGetVar :: Int -> Char -> FExecM FalseData
fGetVar p c = StateT $ \(s,m) -> case Map.lookup c m of
                                Nothing -> throwError $ "Runtime error :: Undefined reference at " ++ show p ++ "!"
                                Just v -> return (v,(s,m))

fPutVar :: Char -> FalseData -> FExecM ()
fPutVar c d = StateT $ \(s,m) -> return ((),(s,Map.insert c d m))

-- type safe pops
fPopNum :: Int -> FExecM Int
fPopNum p = do
        d <- fPop p
        case d of
            FalseNum n -> return n
            _ -> throwError $ "Runtime error :: Expected number on stack at " ++ show p ++ "!"

fPopLambda :: Int -> FExecM [FCodePos]
fPopLambda p = do
        c <- fPop p
        case c of
            FalseLambda l -> return l
            _ -> throwError $ "Runtime error :: Expected Lambda on stack at " ++ show p ++ "!"

fPopRef :: Int -> FExecM Char
fPopRef p = do
        v <- fPop p
        case v of
            FalseVRef r -> return r
            _ -> throwError $ "Runtime error :: Expected reference on stack at " ++ show p ++ "!"

fPopChar :: Int -> FExecM Char
fPopChar p = do
        v <- fPop p
        case v of
            FalseChar r -> return r
            _ -> throwError $ "Runtime error :: Expected character on stack at " ++ show p ++ "!"

fGetNItem :: Int -> Int -> FExecM FalseData
fGetNItem p i = StateT $ \(s,m) -> if i > (length s) - 1
                                 then throwError $ "Runtime error :: Index too high at PICK (pos. " ++ show p ++ ")!"
                                 else return (s !! i,(s,m))

-------------------------------------------
-- Actual FALSE command functions        --
-------------------------------------------

fdup :: Int -> FExecM ()
fdup p = do
        a <- fPop p
        fPush a
        fPush a
        return ()

fdrop :: Int -> FExecM ()
fdrop p = do
        fPop p
        return ()

fswap :: Int -> FExecM ()
fswap p = do
        a <- fPop p
        b <- fPop p
        fPush a
        fPush b
        return ()

frot :: Int -> FExecM ()
frot p = do
        a <- fPop p
        b <- fPop p
        c <- fPop p
        fPush b
        fPush a
        fPush c
        return ()

fpick :: Int -> FExecM ()
fpick p = do
        (FalseNum n) <- fPop p
        it <- fGetNItem p n
        fPush it

fplus :: Int -> FExecM ()
fplus p = do
        a <- fPopNum p
        b <- fPopNum p
        fPush . FalseNum $ a + b

fminus :: Int -> FExecM ()
fminus p = do
        a <- fPopNum p
        b <- fPopNum p
        fPush . FalseNum $ b - a

fmult :: Int -> FExecM ()
fmult p = do
        a <- fPopNum p
        b <- fPopNum p
        fPush . FalseNum $ a * b

fdivide :: Int -> FExecM ()
fdivide p = do
        a <- fPopNum p
        b <- fPopNum p
        fPush . FalseNum $ b `div` a

fnegate :: Int -> FExecM ()
fnegate p = do
        a <- fPopNum p
        fPush . FalseNum $ - a

fand :: Int -> FExecM ()
fand p = do
        a <- fPopNum p
        b <- fPopNum p
        fPush . FalseNum $ if a /= 0 && b /= 0 then (fTrue) else 0

for :: Int -> FExecM ()
for p = do
        a <- fPopNum p
        b <- fPopNum p
        fPush . FalseNum $ if a /= 0 || b /= 0 then (fTrue) else 0

fnot :: Int -> FExecM ()
fnot p = do
        a <- fPopNum p
        fPush . FalseNum $ if a == 0 then (fTrue) else 0

fgreater :: Int -> FExecM ()
fgreater p = do
        a <- fPopNum p
        b <- fPopNum p
        fPush . FalseNum $ if b > a then fTrue else 0

fequal :: Int -> FExecM ()
fequal p = do
        a <- fPopNum p
        b <- fPopNum p
        fPush . FalseNum $ if b == a then fTrue else 0

fexec :: Int -> FExecM ()
fexec p = do
        l@(FalseLambda code) <- fPop p
        falseExec code

fif :: Int -> FExecM ()
fif p = do
        code <- fPopLambda p
        b <- fPopNum p
        if b /= 0
         then falseExec code
         else return ()

fwhile :: Int -> FExecM ()
fwhile p = do
        code <- fPopLambda p
        cond <- fPopLambda p
        falseExec cond
        e <- fPopNum p
        if e /= 0
         then do
                falseExec code
                fPush (FalseLambda cond)
                fPush (FalseLambda code)
                fwhile p
         else return ()

fget :: Int -> FExecM ()
fget p = do
        ref <- fPopRef p
        v <- fGetVar p ref
        fPush v

fput :: Int -> FExecM ()
fput p = do
        ref <- fPopRef p
        d <- fPop p
        fPutVar ref d

fread :: Int -> FExecM ()
fread p = do
        c <- liftIO getChar
        fPush (FalseChar c)

fwrite :: Int -> FExecM ()
fwrite p = do
        c <- fPopChar p
        liftIO (putChar c)

fwritestring :: Int -> String -> FExecM ()
fwritestring p s = do
        liftIO (putStr s)

fprintint :: Int -> FExecM ()
fprintint p = do
        d <- fPopNum p
        liftIO (print d)

fflush :: Int -> FExecM ()
fflush p = liftIO (hFlush stdout)

falseExec :: [FCodePos] -> FExecM ()
falseExec [] = return ()
falseExec a@(cp:cps) = let (c,p) = cp in
                        case c of
                            FNum i -> fPush (FalseNum $ fromIntegral i) >> falseExec cps
                            FChar c -> fPush (FalseChar c) >> falseExec cps
                            FStackOp op -> case op of
                                                FDUP -> fdup p >> falseExec cps
                                                FDROP -> fdrop p >> falseExec cps
                                                FSWAP -> fswap p >> falseExec cps
                                                FROT -> frot p >> falseExec cps
                                                FPICK -> fpick p >> falseExec cps
                            FArithm a -> case a of
                                                FPlus -> fplus p >> falseExec cps
                                                FMinus -> fminus p >> falseExec cps
                                                FMult -> fmult p >> falseExec cps
                                                FDivide -> fdivide p >> falseExec cps
                                                FNegate -> fnegate p >> falseExec cps
                                                FAnd -> fand p >> falseExec cps
                                                FOr -> for p >> falseExec cps
                                                FNot -> fnot p >> falseExec cps
                            FCompare c -> case c of
                                                FGreater -> fgreater p >> falseExec cps
                                                FEqual -> fequal p >> falseExec cps
                            FControl c -> case c of
                                                FLambda code -> fPush (FalseLambda code) >> falseExec cps
                                                FExec -> fexec p >> falseExec cps
                                                FIf -> fif p >> falseExec cps
                                                FWhile -> fwhile p >> falseExec cps
                            FVariableOp o -> case o of
                                                FVarRef c -> fPush (FalseVRef c) >> falseExec cps
                                                FGet -> fget p >> falseExec cps
                                                FPut -> fput p >> falseExec cps
                            FIOOp o -> case o of
                                                FRead -> fread p >> falseExec cps
                                                FWrite -> fwrite p >> falseExec cps
                                                FWriteString s -> fwritestring p s >> falseExec cps
                                                FPrintInt -> fprintint p >> falseExec cps
                                                FFlush -> fflush p >> falseExec cps

fExec :: Code -> IO ()
fExec c = do
        putStrLn "\n---------- I/O ----------"
        result <- runErrorT (runStateT (falseExec code) emptyState)
        case result of
            Left e -> putStrLn e >> enumCode
            Right x -> (void . return $ result)
    where code = case fParse c of
                    Left e -> error e
                    Right c -> c
          enumCode = mapM_ (\(c,n) -> putStrLn $ (printf "%3i" n) ++ " `" ++ c ++ "´") enumChunks
          enumChunks = zip (chunksOf 5 c) ([0,5..] :: [Int])
