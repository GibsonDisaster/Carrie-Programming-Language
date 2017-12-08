module Carrie.Parser.CarrieStructs where  

  data CrValue = CrStringT -- String type
               | CrIntT -- Int type
               | CrFloatT -- Float type
               | CrBoolT -- Boolean type
               | CrNothingT -- Nothing type (void)
               | CrUnknownT -- Unknown type (null)
               | CrListT CrValue -- List type
               | CrString String -- String value
               | CrInt Int -- Int value
               | CrFloat Float -- Float value
               | CrBool Bool -- Boolean value
               | CrNothing () -- Nothing value
               | CrVar String CrValue -- var-name value
               | Greater String String -- val1 val2
               | Lesser String String -- val1 val2
               | Equal String String -- val1 val2
               deriving (Show, Eq)

  data CrStmt = CrAssign String CrStmt -- variable-name and value
              | CrFuncCall String [String] -- func-name and args
              | CrBind String String String -- func-name var output
              | CrDec String CrValue -- var-name type
              | Add String String -- num num
              | Sub String String -- num num
              | Mul String String -- num num
              | Div String String -- num num
              | Return String -- return var
              | If CrValue [CrStmt] -- cond code
              | While CrValue [CrStmt] -- cond code 
              | Comment String -- comment content
              | CrFunc String [CrPair] CrValue [CrStmt] -- name args return-type code
              | CrPrint String -- std print function
              | CrError String -- error for bad parsing results
              | CrList [String] -- List values  
              | CrLiteral String -- Literal value
              | CrMap String String String-- MapFunction function list output
              deriving (Show, Eq)

  type CrPair = (CrValue, String) --val name
