module Main where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import CarrieParser

  main :: IO ()
  main = do
      fileInput <- readFile "test.cr"
      case parse test "" fileInput of
          Left err -> putStrLn (show err)
          Right val -> putStrLn (show val)