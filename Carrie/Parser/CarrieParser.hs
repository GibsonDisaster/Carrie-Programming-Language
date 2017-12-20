module Carrie.Parser.CarrieParser where
    import Text.ParserCombinators.Parsec hiding (spaces)
    import Carrie.Parser.CarrieStructs
    import Control.Monad
    --Written By Henning Tonko ☭
    {-
        TODO:
        • Add in parsing for Floats [_]
        • Add in parsing for Strings [_]
        • Change String reps in CrStmt's to be CrStmt's or CrValue's [_]
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

    _word :: Parser String
    _word = do
       w <- many1 $ choice [string "\"", word]
       return $ concat w

    wordString :: Parser String
    wordString = do
        w <- many1 $ choice [oneOf (['a'..'z']++['A'..'Z']++[' ']), char '"']
        return $ w

    number :: Parser Integer
    number = read <$> many1 digit

    floatNum :: Parser Float
    floatNum = do
        num1 <- many1 digit
        char '.'
        num2 <- many1 digit
        let ans = ((show num1) ++ "." ++ (show num2))
        return $ (read ans :: Float)

    line :: Parser String
    line = do
        l <- many1 (choice [oneOf (['a'..'z']++['A'..'Z']++[' ']++"!#$%&|*+-/:<=>?@^_~"), digit])
        return l

    line' :: Parser CrStmt
    line' = do
        l <- choice [parseMap, parsePrint, parseDec, parseBind, parseAssign, parseFuncCall, parseReturn, parseIf, parseWhile, parseComment]
        optional newline
        return l

    parseList :: Parser CrStmt
    parseList = do
        char '|'
        vals <- many1 $ sepBy1 _word (string ", ")
        char '|'
        return $ CrList $ concat vals

    parseMath :: Parser CrStmt
    parseMath = do
        num1 <- word
        spaces
        op <- oneOf ['+', '-', '*', '/']
        spaces
        num2 <- word
        let op' = case op of
                    '+' -> Add num1 num2
                    '-' -> Sub num1 num2
                    '*' -> Mul num1 num2
                    '/' -> Div num1 num2
        return op'

    parseBind :: Parser CrStmt
    parseBind = do
        string "bind"
        char '('
        f <- word
        char ')'
        char '('
        a <- word
        char ')'
        char '('
        o <- word
        char ')'
        char ';'
        return $ CrBind f a o

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

    parseDec :: Parser CrStmt
    parseDec = do
        string "DEC("
        w <- word
        optional spaces
        char ','
        optional spaces
        t <- word
        optional spaces
        char ')'
        char ';'
        return $ CrDec w (toDataType t)

    parseMap :: Parser CrStmt
    parseMap = do
        string "map("
        f <- word
        optional spaces
        char ','
        optional spaces
        l <- word
        optional spaces
        char ','
        optional spaces
        o <- word
        string ");"
        return $ CrMap f l o

    parseLiteral :: Parser CrStmt
    parseLiteral = do
        char '$'
        --v <- try floatNum
        v1 <- try number
        return $ CrLiteral (show v1)

    parseAssign :: Parser CrStmt
    parseAssign = do
        string "let"
        spaces
        var <- word
        spaces
        string ":="
        spaces
        name <- choice [parseMath, parseLiteral, parseList]
        choice [string ";", string ";\n"]
        return $ CrAssign var name

    parsePrint :: Parser CrStmt
    parsePrint = do
        string "print("
        w <- word
        char ')'
        choice [string ";", string ";\n"]
        return $ CrPrint w

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
        guts <- manyTill line' (string "}")
        return $ guts

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
        string "START\n"
        code <- manyTill line' (string "END")
        return $ If (toBoolStruct cond) code

    parseWhile :: Parser CrStmt
    parseWhile = do
        string "while"
        spaces
        char '('
        cond <- word'
        char ')'
        spaces
        string "START\n"
        code <- manyTill line' (string "END")
        return $ While (toBoolStruct cond) code

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