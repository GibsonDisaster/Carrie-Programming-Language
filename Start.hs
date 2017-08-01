module Start(main) where

import Data.List
import Src
import CarrieCompiler

main = do
    let tokens = findToken "print" (words text)
    parseTokens tokens