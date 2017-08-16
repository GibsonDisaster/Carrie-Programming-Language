module Main where
    import System.IO
    import Data.List
    import System.Process
    import CarrieCompiler

    main :: IO ()
    main = do
        putStrLn "-- Carrie Compiler Version 0.0.4 --"
        g <- openFile "main.cr" ReadMode
        let contents = hGetContents g
        let tokens = fmap (getTokenPairs) (fmap (words) contents)
        c <- openFile "main.c" WriteMode
        hPutStr c "#include <stdio.h>\n"
        hClose c
        tokens >>= parseTokens
        r <- createProcess (proc "gcc" ["-o","Carrie","main.c"])
        putStrLn "Compilation Complete!"