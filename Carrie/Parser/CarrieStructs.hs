module Carrie.Parser.CarrieStructs where

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
              | Greater CrValue CrValue -- val1 val2
              | Lesser CrValue CrValue -- val1 val2
              | Equal CrValue CrValue -- val1 val2
              deriving (Show, Eq)

  type CrPair = (CrValue, String)

-- Typeclass so I can return different structs from same function in a generalized type

  class Carrie c where
    compile :: c -> [String] -- Will turn the struct into its respective code (go, java, c, rust, etc)

-- Instance Declarations for all CrStructs

  instance Carrie CrStruct where
    compile (If _ _) = ["if"]
    compile (IfElse _ _ _) = ["ifelse"]
    compile (While _ _) = ["while"]
    compile (CrFunc _ _ _ _) = ["function"]

  instance Carrie CrValue where
    compile CrStringT = ["String"]
    compile CrIntT = ["Int"]
    compile CrFloatT = ["Float"]
    compile CrBoolT = ["Bool"]
    compile CrNothingT = ["Nothing"]
    compile CrUnknownT = ["Unknown"]
    compile (CrString _) = ["String value"]
    compile (CrInt _) = ["Int Value"]
    compile (CrFloat _) = ["Float Value"]
    compile (CrBool _) = ["Bool Value"]
    compile (CrNothing _) = ["Nothing Value"]
    compile (CrVar _ _) = ["Var Value"]

  instance Carrie CrStmt where
    compile (CrAssign _ _) = ["Assign Stmt"]
    compile (CrAssignV _ _) = ["Variable-Assign Stmt"]
    compile (CrFuncCall _ _) = ["Function Call"]
    compile (Return _) = ["Return Stmt"]
    compile (Greater _ _) = ["Greater-Than Stmt"]
    compile (Lesser _ _) = ["Lesser-Than Stmt"]
    compile (Equal _ _) = ["Equality Stmt"]