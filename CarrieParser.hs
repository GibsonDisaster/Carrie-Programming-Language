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
    word = many1 $ choice [letter, digit]

    number :: Parser Integer
    number = read <$> many1 digit

    line :: Parser String --Change later to, "line :: Parser CrStmt"
    line = do
        l <- many1 (choice [oneOf (['a'..'z']++['A'..'Z']++[' ']++"!#$%&|*+-/:<=>?@^_~"), digit])
        return l

    line' :: Parser CrStmt
    line' = do
        l <- choice [parseAssign, parseFuncCall, parseReturn, parseGreater, parseLesser, parseEqual]
        return l

    parseFuncCall :: Parser CrStmt
    parseFuncCall = do
        string "funcall"
        char '('
        f <- word
        char ')'
        char '('
        a <- sepBy word (choice [string ", ", string ","])
        char ')'
        char ';'
        return $ CrFuncCall f a

    parseAssign :: Parser CrStmt
    parseAssign = do
        string "let"
        spaces
        var <- word
        spaces
        string ":="
        spaces
        name <- word
        return $ CrAssign var name

    parseReturn :: Parser CrStmt
    parseReturn = do
        string "return"
        spaces
        r <- word
        return $ Return r

    parseGreater :: Parser CrStmt
    parseGreater = do
        v1 <- word
        spaces
        char '>'
        spaces
        v2 <- word
        return $ Greater v1 v2

    parseLesser :: Parser CrStmt
    parseLesser = do
        v1 <- word
        spaces
        char '<'
        spaces
        v2 <- word
        return $ Lesser v1 v2

    parseEqual :: Parser CrStmt
    parseEqual = do
        v1 <- word
        spaces
        string "=="
        spaces
        v2 <- word
        return $ Equal v1 v2

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

    funcGuts :: Parser [CrStruct]
    funcGuts = do
        spaces
        char '{'
        newline
        guts <- parseLine
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
        code <- endBy line' (string ";\n")
        optional $ string "}"
        optional newline
        return $ If (toValue cond) code

    parseWhile :: Parser CrStruct
    parseWhile = do
        string "while"
        spaces
        char '('
        cond <- word
        char ')'
        spaces
        string "{\n"
        code <- endBy line' (string ";\n")
        optional $ string "}"
        optional newline
        return $ While (toValue cond) code

    parseLine :: Parser [CrStruct]
    parseLine = do
        ifs <- many parseIf
        whiles <- many parseWhile
        return $ ifs ++ whiles

    parseStmts :: Parser [CrStmt]
    parseStmts = do
        (skipMany space)
        st <- endBy line' (string ";\n")
        return st

    parseFunc :: Parser CrStruct
    parseFunc = do
        n <- funcName
        a <- funcArgs
        t <- funcReturn
        g <- funcGuts
        optional newline
        return $ CrFunc n a t g

    mainParser :: Parser [CrStruct]
    mainParser = do
        fs <- many1 parseFunc
        return fs