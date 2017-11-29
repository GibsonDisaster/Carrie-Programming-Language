module Carrie.Parser.CarrieParser where
    import Text.ParserCombinators.Parsec hiding (spaces)
    import Carrie.Parser.CarrieStructs
    import Control.Monad
    --Made By Henning Tonko ☭
    {-
        TODO:
        • Make parseLine parse more than one line [X]
        • Add funcall()() to valid return types [_]
        • Clean up this code (Seperate files or just move around) [_]
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

    word' :: Parser String
    word' = many1 $ choice [letter, digit, char ' ', char '>', char '<', char '=']

    number :: Parser Integer
    number = read <$> many1 digit

    line :: Parser String --Change later to, "line :: Parser CrStmt"
    line = do
        l <- many1 (choice [oneOf (['a'..'z']++['A'..'Z']++[' ']++"!#$%&|*+-/:<=>?@^_~"), digit])
        return l

    line' :: Parser CrStmt
    line' = do
        l <- choice [parseAssign, parseFuncCall, parseReturn]
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
        choice [string ";", string ";\n"]
        --string ";"
        --choice [string ";\n", string ";", string "\n"]
        return $ CrAssign var name

    parseComment :: Parser CrStmt
    parseComment = do
        string "#"
        spaces
        w <- manyTill anyChar (try (string " #"))
        return $ Comment w

    parseReturn :: Parser CrStmt
    parseReturn = do
        string "return"
        spaces
        r <- word
        choice [string ";", string ";\n"]
        --string ";" -- <|> string "\n"
        return $ Return r

    parseGreater :: Parser CrValue
    parseGreater = do
        v1 <- word
        spaces
        char '>'
        spaces
        v2 <- word
        return $ Greater v1 v2

    parseLesser :: Parser CrValue
    parseLesser = do
        v1 <- word
        spaces
        char '<'
        spaces
        v2 <- word
        return $ Lesser v1 v2

    parseEqual :: Parser CrValue
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

    toBoolStruct :: String -> CrValue
    toBoolStruct expr = if (length ws > 1) then sortExpr ws else (toValue (head ws))
      where
        ws = splitWith (/= ' ') expr
        sortExpr [x, y, z]
          | y == "==" = Equal x z
          | y == ">" = Greater x z
          | y == "<" = Lesser x z
          | otherwise = CrNothing ()

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

    funcGuts :: Parser [CrStmt]
    funcGuts = do
        spaces
        string "{\n"
        guts <- parseLine
        string "\n}"
        return guts

    funcReturn :: Parser CrValue
    funcReturn = do
        spaces
        string "->"
        spaces
        t <- word
        return $ toDataType t

    parseIf :: Parser CrStmt
    parseIf = do
        string "if"
        spaces
        char '('
        cond <- word'
        char ')'
        spaces
        string "{\n"
        code <- endBy line' (char '\n')
        string "}"
        return $ If (toBoolStruct cond) code

    parseWhile :: Parser CrStmt
    parseWhile = do
        string "while"
        spaces
        char '('
        cond <- word'
        char ')'
        spaces
        string "{\n"
        code <- endBy line' (char '\n')
        string "}"
        return $ While (toBoolStruct cond) code

    parseLine :: Parser [CrStmt]
    parseLine = do
        stmts <- sepBy (choice [parseReturn, parseAssign, parseIf, parseWhile]) (string "\n")
        return $ stmts

    parseStmts :: Parser [CrStmt]
    parseStmts = do
        (skipMany space)
        st <- endBy line' (string ";\n")
        return st

    parseFunc :: Parser CrStmt
    parseFunc = do
        n <- funcName
        a <- funcArgs
        t <- funcReturn
        g <- funcGuts
        return $ CrFunc n a t g

    mainParser :: Parser [CrStmt]
    mainParser = do
        fs <- many1 $ choice [parseFunc, parseComment]
        return fs