module FalseLib.Parse where

{-
    Reference documentation: http://strlen.com/false/false.txt
-}

import Data.Char
import Data.Either
import Control.Monad.Error

type FCodePos = (FalseCode,Int)

data FalseCode =
                FNum Int
              | FChar Char
              | FStackOp FalseStackOperation
              | FArithm FalseArithmeticOperation
              | FCompare FalseCompare
              | FControl FalseControl
              | FVariableOp FVar
              | FIOOp FIO
            deriving Show

data FalseStackOperation =
                            FDUP
                          | FDROP
                          | FSWAP
                          | FROT
                          | FPICK
            deriving Show

data FalseArithmeticOperation =
                                FPlus
                              | FMinus
                              | FMult
                              | FDivide
                              | FNegate
                              | FAnd
                              | FOr
                              | FNot
            deriving Show

data FalseCompare = FGreater | FEqual
            deriving Show

data FalseControl =
                    FLambda [FCodePos]
                  | FExec
                  | FIf
                  | FWhile
            deriving Show

data FVar =
             FVarRef Char
           | FGet
           | FPut
            deriving Show

data FIO =
            FRead
          | FWrite
          | FWriteString String
          | FPrintInt
          | FFlush
            deriving Show

type Code = String

falseParse :: Int -> Code -> Either String [FCodePos]
falseParse _ [] = Right []
falseParse p a@(c:cs)
                    | isNumber c = let l = length $ takeWhile isNumber a in
                                    (falseParse (p+l) . dropWhile isNumber $ a) >>= \r -> Right $ (FNum . read . takeWhile isNumber $ a, p) : r
                    | c == '\'' = (falseParse (p+2) . tail $ cs) >>= \r ->  let c' = head cs in
                                                                      if isAlpha c'
                                                                      then Right $ (FChar (head cs), p) : r
                                                                      else throwError $ "Expected letter as char literal; got: '" ++ [c'] ++ "' at " ++ show p
                    | c `elem` "$%\\@ø" = falseParse (p+1) cs >>= \r -> Right $ (case c of
                                                                    '$' -> FStackOp FDUP
                                                                    '%' -> FStackOp FDROP
                                                                    '\\' -> FStackOp FSWAP
                                                                    '@' -> FStackOp FROT
                                                                    '\248' -> FStackOp FPICK -- This is AltGr+o (\248) (Unicode U+00F8) -> ø
                                                                , p) : r
                    | c `elem` "+-*/_&|~" = falseParse (p+1) cs >>= \r -> Right $ (case c of
                                                                    '+' -> FArithm FPlus
                                                                    '-' -> FArithm FMinus
                                                                    '*' -> FArithm FMult
                                                                    '/' -> FArithm FDivide
                                                                    '_' -> FArithm FNegate
                                                                    '&' -> FArithm FAnd
                                                                    '|' -> FArithm FOr
                                                                    '~' -> FArithm FNot
                                                                , p) : r
                    | c `elem` ">=" = falseParse (p+1) cs >>= \r -> Right $ (case c of
                                                                    '>' -> FCompare FGreater
                                                                    '=' -> FCompare FEqual
                                                                , p) : r
                    | c == '[' = let lambdacode = getLambda a
                                     l = length lambdacode
                                     rest = drop (2 + length lambdacode) a
                                 in falseParse (p+1) lambdacode >>= \lc -> falseParse (p+l+2) rest >>= \r -> Right $ (FControl (FLambda lc), p) : r
                    | c `elem` "!?#" = falseParse (p+1) cs >>= \r -> Right $ (case c of
                                                                    '!' -> FControl FExec
                                                                    '?' -> FControl FIf
                                                                    '#' -> FControl FWhile
                                                                , p) : r
                    | isAsciiLower c = falseParse (p+1) cs >>= \r -> Right $ (FVariableOp $ FVarRef c,p) : r
                    | c `elem` ";:" = falseParse (p+1) cs >>= \r -> Right $ (case c of
                                                                    ';' -> FVariableOp FGet
                                                                    ':' -> FVariableOp FPut
                                                                , p) : r
                    | c `elem` "^,.ß" = falseParse (p+1) cs >>= \r -> Right $ (case c of
                                                                    '^' -> FIOOp FRead
                                                                    ',' -> FIOOp FWrite
                                                                    '.' -> FIOOp FPrintInt
                                                                    'ß' -> FIOOp FFlush
                                                                , p) : r
                    | c == '"' = let str = takeWhile (/='"') cs
                                     rest = drop (2 + length str) a
                                     l = length str
                                 in falseParse (p+l+2)rest >>= \r -> Right $ (FIOOp (FWriteString str),p) : r
                    | c == '{' = let c = dropComment a
                                     l = (length a) - (length c)
                                 in falseParse (p+l+4) c
                    | otherwise = falseParse (p+1) cs

getLambda :: Code -> Code
getLambda = init . tail . gl 0
    where gl :: Int -> Code -> Code
          gl _ [] = []
          gl i (c:cs) = case c of
                          '[' -> c : gl (i + 1) cs
                          ']' -> if (i - 1) > 0 then c : gl (i - 1) cs else "]"
                          _ -> if i == 0
                                then []
                                else c : gl i cs

dropComment :: Code -> Code
dropComment = gc 0
    where gc :: Int -> Code -> Code
          gc _ [] = []
          gc i (c:cs) = case c of
                          '{' -> gc (i + 1) cs
                          '}' -> if (i - 1) > 0 then gc (i - 1) cs else cs
                          _ -> if i == 0
                                then cs
                                else gc i cs

fParse :: Code -> Either String [FCodePos]
fParse c = falseParse 0 c
