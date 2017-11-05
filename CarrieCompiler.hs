module CarrieCompiler where
    import Text.ParserCombinators.Parsec hiding (spaces)
    import Control.Monad
    --Made By Henning Tonko ☭
    {-
        TODO:
        • Clean up DataStructs [_]
        • Convert String reps to CrData's, CrLiteral's, etc [_]
        • change stmt parser to return a list of stmt's [_]
        • Add Comments [_]
    -}

    data CrStruct = Stmt CrOp CrData CrLiteral --Operation(add, assign, etc) var value
                  | If CrData [CrData]
                  | IfElse CrData [CrData] [CrData]
                  | While CrData [CrData]
                  | Return CrData
                    deriving (Show)

    instance Eq CrStruct where
        (Stmt _ _ _) == (Stmt _ _ _) = True
        (Stmt _ _ _) == _ = False
        (If _ _) == (If _ _) = True
        (If _ _) == _ = False
        (IfElse _ _ _) == (IfElse _ _ _) = True
        (IfElse _ _ _) == _ = False
        (While _ _) == (While _ _) = True
        (While _ _) == _ = False
        (Return _) == (Return _) = True
        (Return _) == _ = False

    data CrType = CrStringT
                | CrIntT
                | CrFloatT
                | CrBoolT
                | CrUnknown
                deriving (Show, Eq)

    data CrLiteral = CrString String
                   | CrInt Int
                   | CrFloat Float
                   | CrBool Bool
                   deriving (Show, Eq)

    --TODO: finish CrOp list
    data CrOp = Assign
              deriving (Show, Eq)

    data CrData = CrVar String CrLiteral --Name Value
                | CrFunc String [CrPair] CrType [String] --Name args return guts
                deriving Show

    type CrPair = (CrType, String)

    splitWith f xs = case foldr g [[]] xs of {([]:r)-> r; r->r}
        where
            g x r@ ~(s:t) | f x = (x:s):t 
              | null s = r
              | otherwise = []:r

    testInput :: String
    testInput = "henning;\ntonko;\nis;\ncool;\n"

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

    toDataType :: String -> CrType
    toDataType "Int" = CrIntT
    toDataType "String" = CrStringT
    toDataType "Float" = CrFloatT
    toDataType "Bool" = CrBoolT
    toDataType _ = CrUnknown

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

    funcReturn :: Parser CrType
    funcReturn = do
        spaces
        string "->"
        spaces
        t <- word
        return $ toDataType t

    parseStmts :: Parser [String]
    parseStmts = do
        (skipMany space)
        st <- endBy line (string ";\n")
        return st

    combineParsers :: [Parser a] -> Parser [a]
    combineParsers = many1 . choice

    parseFunc :: Parser CrData
    parseFunc = do
        n <- funcName
        a <- funcArgs
        t <- funcReturn
        g <- funcGuts
        return $ CrFunc n a t g

    test :: Parser [CrData]
    test = do
        f1 <- many1 parseFunc
        return f1