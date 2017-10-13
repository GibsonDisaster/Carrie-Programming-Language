module CarrieCompiler where
    import System.IO

    replace :: Char -> Char -> [Char] -> [Char]
    replace z y str = [if x == z then y else x | x <- str]

    parseTokens :: [(String, String)] -> IO ()
    parseTokens arr
     | length arr == 0 = putStr ""
     | length arr == 1 = parsePairs (head arr)
     | otherwise = do
        parsePairs (head arr)
        parseTokens (tail arr)

    getFuncName :: [Char] -> [Char]
    getFuncName str
        | (head str) /= '(' = [(head str)] ++ getFuncName (tail str)
        | otherwise = []

    getFuncArgs :: [Char] -> [Char]
    getFuncArgs str = init (drop (length (getFuncName str) + 1) str)

    getTokenPairs :: [String] -> [(String, String)]
    getTokenPairs [] = []
    getTokenPairs str = [(str !! 0, str !! 1)] ++ getTokenPairs (drop 2 str)

    parsePairs :: (String, String) -> IO ()
    parsePairs (x, y)
        | x == "print" = do
            f <- openFile "main.c" AppendMode
            hPutStrLn f ("printf(" ++ "\"" ++ (replace '/' ' ' y) ++ "\"" ++ ");")
            hClose f
        | x == "funcdec" = do
            f <- openFile "main.c" AppendMode
            hPutStrLn f ("int " ++ (getFuncName y) ++ "(" ++ (replace '+' ',' (replace ':' ' ' (getFuncArgs y))) ++ ")" ++ " {")
            putStrLn "FOUND IT BOYSS"
            hClose f
        | x == "bind" = do
            f <- openFile "main.c" AppendMode
            hPutStr f (y ++ " = ")
            hClose f
        | x == "addint" = do
            f <- openFile "main.c" AppendMode
            hPutStr f (y ++ " + ")
            hClose f
        | x == "with" = do
            f <- openFile "main.c" AppendMode
            hPutStr f (y ++ ";\n")
            hClose f
        | x == "var" = do
            f <- openFile "main.c" AppendMode
            hPutStr f (" " ++ y ++ " ")
            hClose f
        | x == "eq" = do
            f <- openFile "main.c" AppendMode
            hPutStr f y
            hClose f
        | x == "ifstart" = do
            f <- openFile "main.c" AppendMode
            hPutStr f ("if (")
            hClose f
        | x == "boolexpr" = do
            f <- openFile "main.c" AppendMode
            if (y == "then") then (hPutStr f ") {\n") else hPutStr f "} else {\n"
            hClose f
        | x == "ifclose" = do
            f <- openFile "main.c" AppendMode
            hPutStrLn f "}"
            hClose f
        | x == "return" = do
            f <- openFile "main.c" AppendMode
            hPutStr f ("return " ++ y ++ ";\n")
            hClose f
        | x == "forstart" = do
            f <- openFile "main.c" AppendMode
            hPutStr f ("for(" ++ (replace ':' ' ' y) ++ ";")
            hClose f
        | x == "forstop" = do
            f <- openFile "main.c" AppendMode
            hPutStr f (y ++ ";")
            hClose f
        | x == "forfunc" = do
            f <- openFile "main.c" AppendMode
            hPutStrLn f (y ++ ") {")
            hClose f
        | x == "forend" = do
            f <- openFile "main.c" AppendMode
            hPutStrLn f ("}")
            hClose f
        | x == "intdec" = do
            f <- openFile "main.c" AppendMode
            hPutStr f ("int " ++ y ++ " = ")
            hClose f
        | x == "assign" = do
            f <- openFile "main.c" AppendMode
            hPutStrLn f (y ++ ";")
            hClose f
        | x == "funcend" = do
            f <- openFile "main.c" AppendMode
            hPutStrLn f "return 0;"
            hPutStrLn f "}"
            hClose f
        | x == "funccall" = do
            f <- openFile "main.c" AppendMode
            hPutStrLn f (y ++ ";")  
            hClose f
        | x == "CMT" = putStr ""
        | x == "END" = putStr ""
        | x == "DEBUG" = do
            f <- openFile "main.c" AppendMode
            hPutStr f ("printf(\"%i\", " ++ y ++ ");\n")
            hClose f
        | otherwise = putStrLn ("Error parsing token {" ++ x ++ " " ++ y ++ "}")
