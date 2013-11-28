module FalseLib.Exec where

import FalseLib.Parse

import Control.Monad.Error
import Data.Bits
import System.IO
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

data FalseData = FalseNum Int
               | FalseChar Char
               | FalseLambda [FalseCode]
               | FalseVRef Char
               deriving Show

type FStack = [FalseData]
type FMap = Map.Map Char FalseData

type FState = (FStack,FMap)

-- FExecM a => FState -> IO (a,FState)
type FExecM = StateT FState IO

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

fPop :: FExecM FalseData
fPop = StateT $ f
    where f ([],m) = error "Runtime error :: Empty stack!"
          f (s:ss,m) = return (s,(ss,m))

fGetVar :: Char -> FExecM FalseData
fGetVar c = StateT $ \(s,m) -> case Map.lookup c m of
                                Nothing -> error "Runtime error :: Undefined reference!"
                                Just v -> return (v,(s,m))

fPutVar :: Char -> FalseData -> FExecM ()
fPutVar c d = StateT $ \(s,m) -> return ((),(s,Map.insert c d m))

-- type safe pops
fPopNum :: FExecM Int
fPopNum = do
        d <- fPop
        case d of
            FalseNum n -> return n
            _ -> error "Runtime error :: Expected number on stack!"

fPopLambda :: FExecM [FalseCode]
fPopLambda = do
        c <- fPop
        case c of
            FalseLambda l -> return l
            _ -> error "Runtime error :: Expected Lambda on stack!"

fPopRef :: FExecM Char
fPopRef = do
        v <- fPop
        case v of
            FalseVRef r -> return r
            _ -> error "Runtime error :: Expected reference on stack!"

fPopChar :: FExecM Char
fPopChar = do
        v <- fPop
        case v of
            FalseChar r -> return r
            _ -> error "Runtime error :: Expected character on stack!"

fGetNItem :: Int -> FExecM FalseData
fGetNItem i = StateT $ \(s,m) -> return (s !! i,(s,m))

fdup :: FExecM ()
fdup = do
        a <- fPop
        fPush a
        fPush a
        return ()

fdrop :: FExecM ()
fdrop = do
        fPop
        return ()

fswap :: FExecM ()
fswap = do
        a <- fPop
        b <- fPop
        fPush a
        fPush b
        return ()

frot :: FExecM ()
frot = do
        a <- fPop
        b <- fPop
        c <- fPop
        fPush b
        fPush a
        fPush c
        return ()

fpick :: FExecM ()
fpick = do
        (FalseNum n) <- fPop
        it <- fGetNItem (n-1)
        fPush it

fplus :: FExecM ()
fplus = do
        a <- fPopNum
        b <- fPopNum
        fPush . FalseNum $ a + b

fminus :: FExecM ()
fminus = do
        a <- fPopNum
        b <- fPopNum
        fPush . FalseNum $ b - a

fmult :: FExecM ()
fmult = do
        a <- fPopNum
        b <- fPopNum
        fPush . FalseNum $ a * b

fdivide :: FExecM ()
fdivide = do
        a <- fPopNum
        b <- fPopNum
        fPush . FalseNum $ b `div` a

fnegate :: FExecM ()
fnegate = do
        a <- fPopNum
        fPush . FalseNum $ - a

fand :: FExecM ()
fand = do
        a <- fPopNum
        b <- fPopNum
        fPush . FalseNum $ if a /= 0 && b /= 0 then (fTrue) else 0

for :: FExecM ()
for = do
        a <- fPopNum
        b <- fPopNum
        fPush . FalseNum $ if a /= 0 || b /= 0 then (fTrue) else 0

fnot :: FExecM ()
fnot = do
        a <- fPopNum
        fPush . FalseNum $ if a == 0 then (fTrue) else 0

fgreater :: FExecM ()
fgreater = do
        a <- fPopNum
        b <- fPopNum
        fPush . FalseNum $ if b > a then fTrue else 0

fequal :: FExecM ()
fequal = do
        a <- fPopNum
        b <- fPopNum
        fPush . FalseNum $ if b == a then fTrue else 0

fexec :: FExecM ()
fexec = do
        l@(FalseLambda code) <- fPop
        falseExec code

fif :: FExecM ()
fif = do
        code <- fPopLambda
        b <- fPopNum
        if b /= 0
         then falseExec code
         else return ()

fwhile :: FExecM ()
fwhile = do
        code <- fPopLambda
        cond <- fPopLambda
        falseExec cond
        e <- fPopNum
        if e /= 0
         then do
                falseExec code
                fPush (FalseLambda cond)
                fPush (FalseLambda code)
                fwhile
         else return ()

fget :: FExecM ()
fget = do
        ref <- fPopRef
        v <- fGetVar ref
        fPush (FalseVRef ref)
        fPush v

fput :: FExecM ()
fput = do
        ref <- fPopRef
        d <- fPop
        fPutVar ref d

fread :: FExecM ()
fread = do
        c <- liftIO getChar
        fPush (FalseChar c)

fwrite :: FExecM ()
fwrite = do
        c <- fPopChar
        liftIO (putChar c)

fwritestring :: String -> FExecM ()
fwritestring s = do
        liftIO (putStr s)

fprintint :: FExecM ()
fprintint = do
        d <- fPopNum
        liftIO (print d)

fflush :: FExecM ()
fflush = liftIO (hFlush stdout)

falseExec :: [FalseCode] -> FExecM ()
falseExec [] = return ()
falseExec a@(c:cs) = case c of
                        FNum i -> fPush (FalseNum $ fromIntegral i) >> falseExec cs
                        FChar c -> fPush (FalseChar c) >> falseExec cs
                        FStackOp op -> case op of
                                            FDUP -> fdup >> falseExec cs
                                            FDROP -> fdrop >> falseExec cs
                                            FSWAP -> fswap >> falseExec cs
                                            FROT -> frot >> falseExec cs
                                            FPICK -> fpick >> falseExec cs
                        FArithm a -> case a of
                                            FPlus -> fplus >> falseExec cs
                                            FMinus -> fminus >> falseExec cs
                                            FMult -> fmult >> falseExec cs
                                            FDivide -> fdivide >> falseExec cs
                                            FNegate -> fnegate >> falseExec cs
                                            FAnd -> fand >> falseExec cs
                                            FOr -> for >> falseExec cs
                                            FNot -> fnot >> falseExec cs
                        FCompare c -> case c of
                                            FGreater -> fgreater >> falseExec cs
                                            FEqual -> fequal >> falseExec cs
                        FControl c -> case c of
                                            FLambda code -> fPush (FalseLambda code) >> falseExec cs
                                            FExec -> fexec >> falseExec cs
                                            FIf -> fif >> falseExec cs
                                            FWhile -> fwhile >> falseExec cs
                        FVariableOp o -> case o of
                                            FVarRef c -> fPush (FalseVRef c) >> falseExec cs
                                            FGet -> fget >> falseExec cs
                                            FPut -> fput >> falseExec cs
                        FIOOp o -> case o of
                                            FRead -> fread >> falseExec cs
                                            FWrite -> fwrite >> falseExec cs
                                            FWriteString s -> fwritestring s >> falseExec cs
                                            FPrintInt -> fprintint >> falseExec cs
                                            FFlush -> fflush >> falseExec cs

fExec :: [FalseCode] -> IO ()
fExec c = void $ runStateT (falseExec c) emptyState
