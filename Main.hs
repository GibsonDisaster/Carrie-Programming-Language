module Main where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import Carrie.Parser.CarrieParser

  main :: IO ()
  main = do
      fileInput <- readFile "test.cr"
      case parse parseLine "" fileInput of
          Left err -> putStrLn (show err)
          Right val -> putStrLn (show val)