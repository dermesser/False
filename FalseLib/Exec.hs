module FalseLib.Exec where

import FalseLib.Parse

import Data.Bits
import System.IO
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

data FalseData = FalseNum Double
               | FalseChar Char
               | FalseLambda [FalseCode]
               | FalseRef Char
               deriving Show

type FStack = [FalseData]
type FMap = Map.Map Char FalseData

type FState = (FStack,FMap)

type FExecM = StateT FState IO

fTrue :: Double
fTrue = fromIntegral (complement 0 :: Int)

emptyStack :: FStack
emptyStack = []

emptyMap :: FMap
emptyMap = Map.empty

emptyState :: FExecM ()
emptyState = StateT $ \_ -> return ((),(emptyStack,emptyMap))

fPush :: FalseData -> FExecM ()
fPush d = StateT $ \(s,m) -> return ((),(d:s,m))

fPop :: FExecM FalseData
fPop = StateT $ \(s:ss,m) -> return (s,(ss,m))

-- type safe pops
fPopDouble :: FExecM Double
fPopDouble = do
        (FalseNum d) <- fPop
        return d

fPopLambda :: FExecM [FalseCode]
fPopLambda = do
        (FalseLambda c) <- fPop
        return c

fPop2 :: FExecM (FalseData,FalseData)
fPop2 = do
        a <- fPop
        b <- fPop
        return (a,b)

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
        it <- fGetNItem ((round n)-1)
        fPush it

fplus :: FExecM ()
fplus = do
        a <- fPopDouble
        b <- fPopDouble
        fPush . FalseNum $ a + b

fminus :: FExecM ()
fminus = do
        a <- fPopDouble
        b <- fPopDouble
        fPush . FalseNum $ b - a

fmult :: FExecM ()
fmult = do
        a <- fPopDouble
        b <- fPopDouble
        fPush . FalseNum $ a * b

fdivide :: FExecM ()
fdivide = do
        a <- fPopDouble
        b <- fPopDouble
        fPush . FalseNum $ b / a

fnegate :: FExecM ()
fnegate = do
        a <- fPopDouble
        fPush . FalseNum $ - a

fand :: FExecM ()
fand = do
        a <- fPopDouble
        b <- fPopDouble
        fPush . FalseNum $ if a /= 0 && b /= 0 then (fTrue) else 0

for :: FExecM ()
for = do
        a <- fPopDouble
        b <- fPopDouble
        fPush . FalseNum $ if a /= 0 || b /= 0 then (fTrue) else 0

fnot :: FExecM ()
fnot = do
        a <- fPopDouble
        fPush . FalseNum $ if a == 0 then (fTrue) else 0

fgreater :: FExecM ()
fgreater = do
        a <- fPopDouble
        b <- fPopDouble
        fPush . FalseNum $ if b > a then fTrue else 0

fequal :: FExecM ()
fequal = do
        a <- fPopDouble
        b <- fPopDouble
        fPush . FalseNum $ if b == a then fTrue else 0

fexec :: FExecM ()
fexec = do
        (FalseLambda code) <- fPop
        falseExec code

fif :: FExecM ()
fif = do
        code <- fPopLambda
        b <- fPopDouble
        if b /= 0
         then falseExec code
         else return ()

fwhile :: FExecM ()
fwhile = do
        code <- fPopLambda
        cond <- fPopLambda
        falseExec cond
        e <- fPopDouble
        if e /= 0
         then do
                falseExec code
                fPush (FalseLambda cond)
                fPush (FalseLambda code)
                fwhile
         else return ()


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
