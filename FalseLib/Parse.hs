module FalseLib.Parse where

{- From esolangs.org:
    
    Literals

        123 put integer onto the stack
        'c put character code onto the stack 

        Stack

        $ DUP
        % DROP
        \ SWAP
        @ ROT
        ø PICK (dup the nth stack item) 

        Arithmetic

        +

        *
        /
        _ negate (negative numbers are entered "123_")
        & bitwise AND
        | bitwise OR
        ~ bitwise NOT 

        Comparison (false is zero, true is all bits set (-1 or ~0) so that bitwise operators may be used)

        > greater than
        = equals 

        Lambdas and flow control (tests are for non-zero)

        [...] define and put a lambda onto the stack
        ! execute lambda
        ? conditional execute: condition[true]?
        if-else can be expressed as: condition$[\true\]?~[false]? 
        # while loop: [condition][body]# 

            Names

                a-z put a reference to one of the 26 available variables onto the stack
                : store into a variable
                ; fetch from a variable 

                I/O

                ^ read a character (-1 for end-of-input)
                , write a character
                "string" write a string (may contain embedded newlines)
                . write top of stack as a decimal integer
                ß flush buffered input/output 

                Other

                {...} comment
        whitespace is ignored, may be needed to separate consecutive integers 

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
                    | c == '{' = falseParse (drop (2 + length (getComment a)) a)
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

getComment :: Code -> Code
getComment = init . tail . gc 0
    where gc :: Int -> Code -> Code
          gc _ [] = []
          gc i (c:cs) = case c of
                          '{' -> c : gc (i + 1) cs
                          '}' -> c : gc (i - 1) cs
                          _ -> if i == 0
                                then []
                                else c : gc i cs
