module CarrieParser where
    import Text.ParserCombinators.Parsec hiding (spaces)
    import Control.Monad
    --Made By Henning Tonko ☭
    {-
        TODO:
        • Parse CrStruct's [_]
        • Parse Stmt's as [Stmt] [_]
    -}

    data CrStruct = If CrValue [String] -- cond code
                  | IfElse CrValue [CrStmt] [CrStmt] -- cond code (else code)
                  | While CrValue [CrStmt] -- cond code 
                  | CrFunc String [CrPair] CrValue [String] -- name args return-type code (should be [CrStmt])
                    deriving (Show, Eq)

    data CrValue = CrStringT -- String type
                 | CrIntT -- Int type
                 | CrFloatT -- Float type
                 | CrBoolT -- Boolean type
                 | CrNothingT -- Nothing type (void)
                 | CrUnknownT -- Unknown type (null)
                 | CrString String -- String value
                 | CrInt Int -- Int value
                 | CrFloat Float -- Float value
                 | CrBool Bool -- Boolean value
                 | CrNothing () -- Nothing value
                 | CrVar String CrValue -- var-name value
                 deriving (Show, Eq)

    data CrStmt = CrAssign String CrValue -- variable-name and value
                | CrAssignV String CrValue -- variable-name and other variable name (i.e: let x := a)
                | CrFuncCall String [CrValue] -- func-name and args
                | Return CrValue -- return var
                | Greater CrValue CrValue CrValue -- val1 val2 greater-val
                | Lesser CrValue CrValue CrValue -- val1 val2 lesser-val
                | Equal CrValue CrValue Bool -- val1 val2 equal?
                deriving (Show, Eq)

    type CrPair = (CrValue, String)

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