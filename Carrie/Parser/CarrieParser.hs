module Carrie.Parser.CarrieParser where
    import Text.ParserCombinators.Parsec hiding (spaces)
    import Carrie.Parser.CarrieStructs
    import Control.Monad
    --Made By Henning Tonko ☭
    {-
        TODO:
        • Parse CrStruct's [_]
        • Parse Stmt's as [Stmt] [_]
    -}

    splitWith :: Foldable t => (a -> Bool) -> t a -> [[a]]
    splitWith f xs = case foldr g [[]] xs of {([]:r)-> r; r->r}
        where
            g x r@ ~(s:t) | f x = (x:s):t 
              | null s = r
              | otherwise = []:r

    spaces :: Parser ()
    spaces = skipMany1 space

    symbol :: Parser Char
    symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

    word :: Parser String
    word = many1 letter

    number :: Parser Integer
    number = read <$> many1 digit

    line :: Parser String
    line = do
        l <- many1 (choice [oneOf (['a'..'z']++['A'..'Z']++[' ']++"!#$%&|*+-/:<=>?@^_~"), digit])
        return l

    toDataType :: String -> CrValue
    toDataType "Int" = CrIntT
    toDataType "String" = CrStringT
    toDataType "Float" = CrFloatT
    toDataType "Bool" = CrBoolT
    toDataType "Nothing" = CrNothingT
    toDataType _ = CrUnknownT

    toValue :: String -> CrValue
    toValue "True" = CrBool True
    toValue "False" = CrBool False
    toValue _ = CrNothing ()

    funcName :: Parser String
    funcName = do
        skipMany spaces
        skipMany newline
        string "func"
        skipMany spaces
        name <- word
        return name

    funcArgs :: Parser [CrPair]
    funcArgs = do
        char '('
        args <- many (noneOf [')'])
        char ')'
        let args' = splitWith (/= ' ') args
        let args'' = map (splitWith (/= ':')) args'
        let args''' = map (\[t, v] -> (toDataType t, v)) args''
        return args'''

    funcGuts :: Parser [String]
    funcGuts = do
        spaces
        char '{'
        newline
        guts <- parseStmts
        char '}'
        return guts

    funcReturn :: Parser CrValue
    funcReturn = do
        spaces
        string "->"
        spaces
        t <- word
        return $ toDataType t

    parseIf :: Parser CrStruct
    parseIf = do
        string "if"
        spaces
        char '('
        cond <- word
        char ')'
        spaces
        string "{\n"
        code <- endBy line (string ";\n")
        string "}\n"
        optional newline
        return $ If (toValue cond) code

    parseLine :: Parser [CrStruct]
    parseLine = do
        ifs <- many1 parseIf
        return ifs

    parseStmts :: Parser [String]
    parseStmts = do
        (skipMany space)
        st <- endBy line (string ";\n")
        return st

    combineParsers :: [Parser a] -> Parser [a]
    combineParsers = many1 . choice

    parseFunc :: Parser CrStruct
    parseFunc = do
        n <- funcName
        a <- funcArgs
        t <- funcReturn
        g <- funcGuts
        return $ CrFunc n a t g

    test :: Parser [CrStruct]
    test = do
        f1 <- many1 parseFunc
        return f1