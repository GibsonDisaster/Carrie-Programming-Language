module Main where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import System.Environment
  import Carrie.Parser.CarrieParser
  import Carrie.Parser.CarrieStructs

  main :: IO ()
  main = do
      fileName <- fmap head getArgs
      fileInput <- readFile fileName
      case parse mainParser "" fileInput of
          Left err -> putStrLn (show err)
          Right val -> putStrLn (show val)