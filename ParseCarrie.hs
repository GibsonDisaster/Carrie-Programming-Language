module ParseCarrie where
  import System.IO
  import Control.Monad
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  data BExpr = BoolConst Bool
             | Not BExpr
             | BBinary BBinOp BExpr BExpr
             | RBinary RBinOp AExpr AExpr
             deriving (Show)
    
  data BBinOp = And | Or deriving (Show)

  data RBinOp = Greater | Less | Equal deriving (Show)

  data AExpr = Var String
             | IntConst Integer
             | Neg AExpr
             | ABinary ABinOp AExpr AExpr
             deriving (Show)

  data ABinOp = Add
              | Subtract
              | Multiply
              | Divide
              deriving (Show)

  data Stmt = Seq [Stmt]
            | Assign String AExpr
            | If BExpr Stmt Stmt
            | While BExpr Stmt
            | Func String [String] [String]
            deriving (Show)

  languageDef = emptyDef {
                            Token.commentStart = "/*",
                            Token.commentEnd = "*/",
                            Token.commentLine = "//",
                            Token.identStart = letter,
                            Token.identLetter = alphaNum,
                            Token.reservedNames = ["if", "then", "else", "while", "do", "true", "false", "not", "and", "or", "funcdec"],
                            Token.reservedOpNames = [ "+", "-", "*", "/", ":=", "==", "<", ">", "and", "or", "not"]
                         }

  lexer = Token.makeTokenParser languageDef
  
  identifier = Token.identifier lexer
  reserved   = Token.reserved   lexer
  reservedOp = Token.reservedOp lexer
  parens     = Token.parens     lexer
  integer    = Token.integer    lexer
  semi       = Token.semi       lexer
  whiteSpace = Token.whiteSpace lexer

  carrieParser :: Parser Stmt
  carrieParser = whiteSpace >> statement

  statement :: Parser Stmt
  statement = parens statement <|> sequenceOfStmt

  sequenceOfStmt = do
    list <- (sepBy1 statement' semi)
    return $ if length list == 1 then head list else Seq list

  statement' :: Parser Stmt
  statement' = funcStmt <|> ifStmt <|> whileStmt <|> assignStmt

  betweenBraces :: Parser [String]
  betweenBraces = do
    char '{'
    b <- manyTill (many1 (noneOf "}")) (char '}')
    return $ b

  betweenParens :: Parser [String]
  betweenParens = do
    char '('
    b <- manyTill (many1 (noneOf ")")) (char ')')
    return $ b

  word :: Parser String
  word = many1 letter

  funcStmt :: Parser Stmt
  funcStmt = do
    reserved "funcdec"
    _ <- spaces
    name <- word
    args <- betweenParens
    _ <- spaces
    block <- betweenBraces
    return $ Func name args block

  ifStmt :: Parser Stmt
  ifStmt = do
    reserved "if"
    cond <- bExpression
    reserved "then"
    stmt1 <- statement
    reserved "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2

  whileStmt :: Parser Stmt
  whileStmt = do
    reserved "while"
    cond <- bExpression
    reserved "do"
    stmt <- statement
    return $ While cond stmt
  
  assignStmt :: Parser Stmt
  assignStmt = do
    var <- identifier
    reservedOp ":="
    expr <- aExpression
    return $ Assign var expr

  aExpression :: Parser AExpr
  aExpression = buildExpressionParser aOperators aTerm
  
  bExpression :: Parser BExpr
  bExpression = buildExpressionParser bOperators bTerm
  
  aOperators = [ [Prefix (reservedOp "-"   >> return (Neg))]
                , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft]
                , [Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
                , [Infix  (reservedOp "+"   >> return (ABinary Add)) AssocLeft]
                , [Infix (reservedOp "-" >> return (ABinary Subtract)) AssocLeft]
                ]
  
  bOperators = [ [Prefix (reservedOp "not" >> return (Not))]
                , [Infix (reservedOp "and" >> return (BBinary And)) AssocLeft]
                , [Infix (reservedOp "or"  >> return (BBinary Or)) AssocLeft]
                ]

  aTerm =  parens aExpression
        <|> liftM Var identifier
        <|> liftM IntConst integer
  
  bTerm =  parens bExpression
        <|> (reserved "true"  >> return (BoolConst True ))
        <|> (reserved "false" >> return (BoolConst False))
        <|> rExpression
  
  rExpression = do
    a1 <- aExpression
    op <- relation
    a2 <- aExpression
    return $ RBinary op a1 a2
  
  relation =  (reservedOp ">" >> return Greater) <|> (reservedOp "<" >> return Less) <|> (reservedOp "==" >> return Equal)
  
  parseString :: String -> Stmt
  parseString str = case parse carrieParser "" str of
    Left e  -> error $ show e
    Right r -> r
  
  parseFile :: String -> IO Stmt
  parseFile file = do
    program <- readFile file
    case parse carrieParser "" program of
      Left e  -> print e >> fail "parser error"
      Right r -> return r