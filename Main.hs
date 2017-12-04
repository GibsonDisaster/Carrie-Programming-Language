module Main where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import System.Environment
  import System.Process
  import Carrie.Parser.CarrieParser
  import Carrie.Parser.CarrieStructs
  import Carrie.Compiler.CarrieCompiler

  main :: IO ()
  main = do
    fileName <- fmap head getArgs
    fileInput <- readFile fileName
    let p = case parse mainParser "" fileInput of
                Left err -> [CrError "Something went wrong! Check your code!"]
                Right val -> val
    writeFile "main.rs" ""
    appendFile "main.rs" (concat $ map compile p)
    runCommand "rustc main.rs"
    putStrLn "Compiling has finished! (main.rs has been created)"