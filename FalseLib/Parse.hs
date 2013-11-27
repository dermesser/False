module FalseLib.Parse where

{-
    Reference documentation: http://strlen.com/false/false.txt
-}

import Data.Char

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

falseParse :: Code -> [FalseCode]
falseParse [] = []
falseParse a@(c:cs) | isNumber c = (FNum . read . takeWhile isNumber $ a) : (falseParse . dropWhile isNumber $ a)
                    | c == '\'' = (FChar . head $ cs) : (falseParse . tail $ cs)
                    | c `elem` "$%\\@ø" = case c of
                                                '$' -> FStackOp FDUP
                                                '%' -> FStackOp FDROP
                                                '\\' -> FStackOp FSWAP
                                                '@' -> FStackOp FROT
                                                '\248' -> FStackOp FPICK -- This is AltGr+o (\248) (Unicode U+00F8) -> ø
                                            : falseParse cs
                    | c `elem` "+-*/_&|~" = case c of
                                                '+' -> FArithm FPlus
                                                '-' -> FArithm FMinus
                                                '*' -> FArithm FMult
                                                '/' -> FArithm FDivide
                                                '_' -> FArithm FNegate
                                                '&' -> FArithm FAnd
                                                '|' -> FArithm FOr
                                                '~' -> FArithm FNot
                                            : falseParse cs
                    | c `elem` ">=" = case c of
                                                '>' -> FCompare FGreater
                                                '=' -> FCompare FEqual
                                            : falseParse cs
                    | c == '[' = let lambdacode = getLambda a
                                     rest = drop (2 + length lambdacode) a
                                 in FControl (FLambda (falseParse lambdacode)) : falseParse rest
                    | c `elem` "!?#" = case c of
                                                '!' -> FControl FExec
                                                '?' -> FControl FIf
                                                '#' -> FControl FWhile
                                            : falseParse cs
                    | c >= 'a' && c <= 'z' = FVariableOp (FVarRef c) : falseParse cs
                    | c `elem` ";:" = case c of
                                                ';' -> FVariableOp FGet
                                                ':' -> FVariableOp FPut
                                            : falseParse cs
                    | c `elem` "^,.ß" = case c of
                                                '^' -> FIOOp FRead
                                                ',' -> FIOOp FWrite
                                                '.' -> FIOOp FPrintInt
                                                'ß' -> FIOOp FFlush
                                            : falseParse cs
                    | c == '"' = let str = takeWhile (/='"') cs
                                     rest = drop (2 + length str) a
                                 in FIOOp (FWriteString str) : falseParse rest                                           
                    | c == '{' = falseParse (drop (2 + length (dropComment a)) a)
                    | otherwise = falseParse cs

getLambda :: Code -> Code
getLambda = init . tail . gl 0
    where gl :: Int -> Code -> Code
          gl _ [] = []
          gl i (c:cs) = case c of
                          '[' -> c : gl (i + 1) cs
                          ']' -> c : gl (i - 1) cs
                          _ -> if i == 0
                                then []
                                else c : gl i cs

dropComment :: Code -> Code
dropComment = gc 0
    where gc :: Int -> Code -> Code
          gc _ [] = []
          gc i (c:cs) = case c of
                          '{' -> gc (i + 1) cs
                          '}' -> gc (i - 1) cs
                          _ -> if i == 0
                                then cs
                                else gc i cs
