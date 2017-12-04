module Carrie.Compiler.CarrieCompiler where
  import Carrie.Parser.CarrieStructs
  import Data.List

  testAssign = CrAssign "x" (Add "10" "0")

  testFuncCall = CrFuncCall "main" ["x", "y"]

  testReturn = Return "Henning"

  testFunction = CrFunc "fact" [(CrIntT,"x"), (CrBoolT, "y")] CrIntT [Return "2"]

  testFactorial = CrFunc "fact" [(CrIntT,"x")] CrIntT [If (Equal "x" "1") [Return "1"],CrAssign "y" (Sub "x" "1"),CrDec "z" CrIntT,CrBind "fact" "y" "z",CrAssign "f" (Mul "z" "x"),Return "f"]

  clearFile :: IO ()
  clearFile = do
    writeFile "main.rs" "fn main() {\nprintln!(\"{}\", fact(4));\n}"

  -- General type class used for really no reason

  class Carrie c where
    compile :: c -> String -- Will turn the struct into its respective code (go, java, c, rust, etc)

  instance Carrie CrValue where
    compile CrIntT = "i32"
    compile CrBoolT = "bool"
    compile CrStringT = "&str"
    compile CrFloatT = "f32"
    compile (CrListT t) = "Vec<" ++ (compile t) ++ ">"
    compile CrNothingT = ""
    compile CrUnknownT = "ERROR UNKNOWN VALUE"
    compile (CrString s) = s
    compile (CrInt i) = show i
    compile (CrFloat f) = show f
    compile (CrBool b) = show b
    compile (CrList l) = concat $ map compile l
    compile (CrNothing n) = "()"
    compile (CrVar n v) = n
    compile (Greater n m) = n ++ " > " ++ m
    compile (Lesser n m) = n ++ " < " ++ m
    compile (Equal n m) = n ++ " == " ++ m

  instance Carrie CrStmt where
    compile (Add s1 s2) = s1 ++ " + " ++ s2
    compile (Sub n m) = n ++ " - " ++ m
    compile (Mul n m) = n ++ " * " ++ m
    compile (Div n m) = n ++ " / " ++ m
    compile (CrDec n t) = "\nlet " ++ n ++ ": " ++ (compile t) ++ ";"
    compile (If v code) = "\nif (" ++ (compile v) ++ ") {\n" ++ (concat $ map compile code) ++ "\n}"
    compile (Return r) = "\nreturn " ++ r ++ ";"
    compile (CrAssign name val) = "\nlet " ++ name ++ " = " ++ (compile val) ++ ";"
    compile (CrFuncCall name args) = "\nname" ++ "(" ++ (concat $ intersperse ", " args) ++ ");"
    compile (CrBind n v o) = "\nlet " ++ o ++ " = " ++ n ++ "(" ++ v ++ ")" ++ ";" --Potential problem with multiple inputs. CHECK OUT SOON
    compile (CrFunc name args returnT code) = "\nfn "
                                              ++ name
                                              ++ "("
                                              ++ (concat $ intersperse ", " (map (\(t, v) -> v ++ ": " ++ compile t) args))
                                              ++ (if (returnT /= CrNothingT) then (") -> " ++ (compile returnT)) else ") ")
                                              ++ " {\n"
                                              ++ (concat $ map compile code)
                                              ++ "\n}"
    compile (Comment c) = "\n// " ++ c
    compile (CrPrint w) = "println!(\"{}\", " ++ w ++ ");"
