module Start(main) where

import Data.List
import System.IO
import System.Process
import Src
import CarrieCompiler

main :: IO ()
main = do
    putStrLn "-- Carrie Compiler Version 0.0.3 --"
    let tokens = tokenPairs (words text)
    c <- openFile "main.c" WriteMode
    hPutStr c "#include <stdio.h>\n"
    hClose c
    parseTokens tokens
    r <- createProcess (proc "gcc" ["main.c"])
    putStrLn "Compilation Complete!"