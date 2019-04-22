-- Nexessary Imports

import Lexer
import Parser
import Data.List (genericIndex)


data LLVM = LLVM_IR (String) (String) (String)

-- Functions

main_ :: String
main_ = "\n\n" ++ "define i32 @main()" ++ "\n{" ++ "\n\t" ++ "ret i32 0" ++ "\n}"

codegen (Prog p) = (snd (codegenProg p)) ++ (fst (codegenProg p)) ++ main_

codegenProg [x] = (fst (codegenDec x), snd (codegenDec x))
codegenProg (h:t) = ((fst (codegenDec h)) ++ (fst (codegenProg t)), (snd (codegenDec h)) ++ (snd (codegenProg t)))

codegenDec :: DEC -> (String, String)
codegenDec (DEF s p b) = (("\n" ++ "define void @" ++ s ++ "(" ++ codegenParams p ++ ") " ++ (fst (codegenBlock b))), snd (codegenBlock b))

codegenParams [] = ""
codegenParams [Param type_ s] = (getParamType type_) ++ "%" ++ s
codegenParams (h:t) = (codegenParams [h]) ++ ", " ++ codegenParams t

getParamType (Int_) = "i32 "
getParamType (Bool_) = "i1 zeroext "
getParamType (String_) = "i8* "

codegenBlock (Block exps) = (("\n{" ++ (fst (codegenExps 0 exps)) ++ "\n\tret void" ++ "\n}"), snd (codegenExps 0 exps))

codegenExps count [] = ("", "")
codegenExps count [x] = (("\n\t" ++ (fst (codegenE count x))), snd (codegenE count x))
codegenExps count (h:t) = (("\n\t" ++ (fst (codegenE count h)) ++ (fst (codegenExps count t))), ((snd (codegenE count h)) ++ (snd (codegenExps count t))))

codegenE count (INT x) = (("%1 = alloca i32, align 4" ++ "\n\t"  ++ "store i32 " ++ (show x) ++ ", i32* %1"), "")
codegenE count (STR s) = ("", ("@.str = private constant [" ++ (show ((length s))) ++ " x i8] c" ++ stringIR s ++ "\n"))
codegenE count (BinOp bo) = (codegenBinop count bo, "")

codegenBinop count (Mult (INT x) (INT y)) = "%" ++ (show (count + 1)) ++ " = mul i32 " ++ (show x) ++ ", " ++ (show y) ++ "\n\t"
codegenBinop count (Mult (ID x) (ID y)) = "%" ++ (show (count + 1)) ++ " = mul i32 %" ++ x ++ ", %" ++ y ++ "\n\t"
codegenBinop count (Mult (INT x) (ID y)) = "%" ++ (show (count + 1)) ++ " = mul i32 " ++ (show x) ++ ", %" ++ y ++ "\n\t"
codegenBinop count (Mult (ID x) (INT y)) = "%" ++ (show (count + 1)) ++ " = mul i32 %" ++ x ++ ", " ++ (show y) ++ "\n\t"
codegenBinop count (Mult (INT x) y)
  | count == 0 = (fst (codegenE (count) y)) ++ "%" ++ (show ((read [(fst (codegenE (count+1) y))!!1] :: Integer))) ++ " = mul i32 " ++ (show x) ++ ", %" ++ (show ((read [(fst (codegenE (count+1) y))!!1] :: Integer) - 1)) ++ "\n\t"
  | otherwise = (fst (codegenE (count + 1) y)) ++ "%" ++ (show ((read [(fst (codegenE (count + 1) y))!!1] :: Integer) + 1)) ++ " = mul i32 " ++ (show x) ++ ", %" ++ [(fst (codegenE count y))!!1] ++ "\n\t"
codegenBinop count (Mult (ID x) y) = (fst (codegenE (count) y)) ++ "%" ++ (show ((read [(fst (codegenE (count+1) y))!!1] :: Integer) + 1)) ++ " = mul i32 %" ++ x ++ ", %" ++ [(fst (codegenE count y))!!1] ++ "\n\t"

stringIR [x, '"'] = x:"\\0A\\00\""
stringIR (h:t) = h:(stringIR t)



