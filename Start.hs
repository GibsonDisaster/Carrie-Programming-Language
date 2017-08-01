module Start(main) where

import Data.List
import System.IO
import Src
import CarrieCompiler

main = do
    let tokens = tokenPairs (words text)
    c <- openFile "main.c" WriteMode
    hPutStr c ""
    hClose c
    f <- openFile "main.c" AppendMode
    hPutStrLn f "#include <stdio.h>"
    hClose f
    parseTokens tokens