-- {Compiler For Carrie Programming Language Version 0.0.1} --
module CarrieCompiler(findToken, getIndex, addSpace, parsePairs, parseTokens) where

import Data.List
findToken :: String -> [String] -> [(String, String)]
findToken t arr
    | arr == [] = [("END", "END")]
    | t `elem` arr = [(t, arr !! ((getIndex t arr) + 1))] ++ findToken t (delete (arr !! ((getIndex t arr) + 1)) (delete t arr))
    | otherwise = [("END", "END")]

getIndex :: Eq a => a -> [a] -> Int
getIndex n arr
    | length arr > 1 = head [y | (x, y) <- zip arr [0..length arr], x == n]
    | otherwise = 0

replace :: Char -> Char -> [Char] -> [Char]
replace x y str = [if x == x then y else x | x <- str]

parseTokens :: [(String, String)] -> IO ()
parseTokens [] = putStr ""
parseTokens (x:xs) = do
    parsePairs x
    parseTokens xs

parsePairs :: (String, String) -> IO ()
parsePairs (x, y)
    | x == "print" = putStrLn y
    | x == "END" || y == "END" = putStr ""
    | otherwise = putStrLn ("Error parsing token {" ++ x ++ " " ++ y ++ "}")

 
