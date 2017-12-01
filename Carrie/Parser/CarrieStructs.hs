module Carrie.Parser.CarrieStructs where

  data CrValue = CrStringT -- String type
               | CrIntT -- Int type
               | CrFloatT -- Float type
               | CrBoolT -- Boolean type
               | CrListT -- List type
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
              deriving (Show, Eq)

  type CrPair = (CrValue, String) --val name

-- Typeclass so I can return different structs from same function in a generalized type
-- Also will create a type classed method for turning structs into actual code.

  class Carrie c where
    compile :: c -> [String] -- Will turn the struct into its respective code (go, java, c, rust, etc)

  instance Carrie CrValue where
    compile CrStringT = ["String"]
    compile CrIntT = ["Int"]
    compile CrFloatT = ["Float"]
    compile CrBoolT = ["Bool"]
    compile CrNothingT = ["Nothing"]
    compile CrUnknownT = ["Unknown Value"]
    compile (CrString _) = ["String value"]
    compile (CrInt _) = ["Int Value"]
    compile (CrFloat _) = ["Float Value"]
    compile (CrBool _) = ["Bool Value"]
    compile (CrNothing _) = ["Nothing Value"]
    compile (CrVar _ _) = ["Var Value"]
    compile (Greater _ _) = ["Greater-Than Stmt"]
    compile (Lesser _ _) = ["Lesser-Than Stmt"]
    compile (Equal _ _) = ["Equality Stmt"]

  instance Carrie CrStmt where
    compile (CrAssign _ _) = ["Assign Stmt"]
    compile (CrFuncCall _ _) = ["Function Call"]
    compile (Return _) = ["Return Stmt"]
    compile (If _ _) = ["if"]
    compile (While _ _) = ["while"]
    compile (CrFunc _ _ _ _) = ["function"]