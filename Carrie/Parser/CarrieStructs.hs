module Carrie.Parser.CarrieStructs where  

  data CrValue = CrStringT -- String type
               | CrIntT -- Int type
               | CrFloatT -- Float type
               | CrBoolT -- Boolean type
               | CrListT CrValue -- List type
               | CrNothingT -- Nothing type (void)
               | CrUnknownT -- Unknown type (null)
               | CrString String -- String value
               | CrInt Int -- Int value
               | CrFloat Float -- Float value
               | CrBool Bool -- Boolean value
               | CrList [CrValue] -- List values
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
              | Add String String
              | Sub String String
              | Mul String String
              | Div String String
              | Return String -- return var
              | If CrValue [CrStmt] -- cond code
              | While CrValue [CrStmt] -- cond code 
              | Comment String -- comment content
              | CrFunc String [CrPair] CrValue [CrStmt] -- name args return-type code (should be [CrStmt])
              | CrPrint String -- std print function
              | CrError String -- error for bad parsing results
              deriving (Show, Eq)

  type CrPair = (CrValue, String) --val name