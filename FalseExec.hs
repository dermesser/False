module Main where

import System.IO
import FalseLib.Exec
import FalseLib.Parse

main = do
    putStrLn "Haskell FALSE interpreter\nWaiting for code... (end with Ctrl-D)"
    code <- readCode
    fExec code

readCode :: IO Code
readCode = do
    eof <- isEOF
    if eof
     then return ""
     else do
        ln <- getLine
        lns <- readCode
        return (ln++lns)
