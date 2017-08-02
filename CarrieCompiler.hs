-- {Compiler For Carrie Programming Language Version 0.0.3} --
module CarrieCompiler(findToken, getIndex, replace, parsePairs, parseTokens, tokenPairs) where

import Data.List
import System.IO

-- Deprecated code I can't bare to delete {
findToken :: String -> [String] -> [(String, String)]
findToken t arr
    | arr == [] = [("END", "END")]
    | t `elem` arr = [(t, arr !! ((getIndex t arr) + 1))] ++ findToken t (delete (arr !! ((getIndex t arr) + 1)) (delete t arr))
    | otherwise = [("END", "END")]

getIndex :: Eq a => a -> [a] -> Int
getIndex n arr
    | length arr > 1 = head [y | (x, y) <- zip arr [0..length arr], x == n]
    | otherwise = 0
-- }

replace :: Char -> Char -> [Char] -> [Char]
replace z y str = [if x == z then y else x | x <- str]

parseTokens :: [(String, String)] -> IO ()
parseTokens [] = putStr ""
parseTokens (x:xs) = do
    parsePairs x
    parseTokens xs

parsePairs :: (String, String) -> IO ()
parsePairs (x, y)
    | x == "print" = do
        f <- openFile "main.c" AppendMode
        hPutStrLn f ("printf(" ++ "\"" ++ (replace '/' ' ' y) ++ "\"" ++ ");")
        hClose f
    | x == "funcdec" = do
        f <- openFile "main.c" AppendMode
        hPutStrLn f ("int " ++ y ++ "() {")
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
    | x == "intdec" = do
        f <- openFile "main.c" AppendMode
        hPutStr f ("int " ++ y ++ ";\n")
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
    | x == "CMT" = putStr ""
    | x == "END" = putStr ""
    | otherwise = putStrLn ("Error parsing token {" ++ x ++ " " ++ y ++ "}")

tokenPairs :: [String] -> [(String, String)]
tokenPairs [] = []
tokenPairs arr = [(head arr, head(tail arr))] ++ tokenPairs (drop 2 arr)