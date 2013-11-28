module FalseLib.Parse where

{-
    Reference documentation: http://strlen.com/false/false.txt
-}

import Data.Char
import Data.Either
import Control.Monad.Error

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
                    FLambda [FalseCode]
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

falseParse :: Code -> Either String [FalseCode]
falseParse [] = Right []
falseParse a@(c:cs) | isNumber c = (falseParse . dropWhile isNumber $ a) >>= \r -> Right $ (FNum . read . takeWhile isNumber $ a) : r
                    | c == '\'' = (falseParse . tail $ cs) >>= \r ->  let c' = head cs in
                                                                      if isAlpha c'
                                                                      then Right $ FChar (head cs) : r
                                                                      else throwError $ "Expected letter as char literal; got: '" ++ [c'] ++ "' at " ++ take 5 a ++ " ..."
                    | c `elem` "$%\\@ø" = falseParse cs >>= \r -> Right $ case c of
                                                                    '$' -> FStackOp FDUP
                                                                    '%' -> FStackOp FDROP
                                                                    '\\' -> FStackOp FSWAP
                                                                    '@' -> FStackOp FROT
                                                                    '\248' -> FStackOp FPICK -- This is AltGr+o (\248) (Unicode U+00F8) -> ø
                                                                : r
                    | c `elem` "+-*/_&|~" = falseParse cs >>= \r -> Right $ case c of
                                                                    '+' -> FArithm FPlus
                                                                    '-' -> FArithm FMinus
                                                                    '*' -> FArithm FMult
                                                                    '/' -> FArithm FDivide
                                                                    '_' -> FArithm FNegate
                                                                    '&' -> FArithm FAnd
                                                                    '|' -> FArithm FOr
                                                                    '~' -> FArithm FNot
                                                                : r
                    | c `elem` ">=" = falseParse cs >>= \r -> Right $ case c of
                                                                    '>' -> FCompare FGreater
                                                                    '=' -> FCompare FEqual
                                                                : r
                    | c == '[' = let lambdacode = getLambda a
                                     rest = drop (2 + length lambdacode) a
                                 in falseParse lambdacode >>= \lc -> falseParse rest >>= \r -> Right $ (FControl $ FLambda lc) : r
                    | c `elem` "!?#" = falseParse cs >>= \r -> Right $ case c of
                                                                    '!' -> FControl FExec
                                                                    '?' -> FControl FIf
                                                                    '#' -> FControl FWhile
                                                                : r
                    | isAsciiLower c = falseParse cs >>= \r -> Right $ FVariableOp (FVarRef c) : r
                    | c `elem` ";:" = falseParse cs >>= \r -> Right $ case c of
                                                                    ';' -> FVariableOp FGet
                                                                    ':' -> FVariableOp FPut
                                                                : r
                    | c `elem` "^,.ß" = falseParse cs >>= \r -> Right $ case c of
                                                                    '^' -> FIOOp FRead
                                                                    ',' -> FIOOp FWrite
                                                                    '.' -> FIOOp FPrintInt
                                                                    'ß' -> FIOOp FFlush
                                                                : r
                    | c == '"' = let str = takeWhile (/='"') cs
                                     rest = drop (2 + length str) a
                                 in falseParse rest >>= \r -> Right $ FIOOp (FWriteString str) : r
                    | c == '{' = falseParse (dropComment a)
                    | otherwise = falseParse cs

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
                          '}' -> if (i - 1) > 0 then gc (i - 1) cs else []
                          _ -> if i == 0
                                then cs
                                else gc i cs
