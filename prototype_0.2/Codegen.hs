-- Nexessary Imports

import Lexer
import Parser

-- Functions

codegenProg :: PROG -> String
codegenProg (P dec) = codegenDec dec

codegenDec :: DEC -> String
codegenDec (DEF s v b) = "define void @" ++ s ++ "(" ++ codegenVardec v ++ ") " ++ codegenBlock b

codegenVardec None = ""
codegenVardec (One s) = "i32 %" ++ s
codegenVardec (Many s rest) = "i32 %" ++ s ++ ", " ++ codegenVardec rest

codegenBlock (Block ene) = "{" ++ codegenEne ene ++ "\n\tret void\n}"

codegenEne (Single e) = codegenE e

codegenE (ASSIGN s (INT x)) = "\n\t" ++ "%" ++ s ++ " = " ++ "alloca i32\n" ++ "\tstore i32 " ++ (show x) ++ ", i32* %" ++ s
codegenE (ASSIGN s (STR s_)) = "\n\t" ++ "%" ++ s ++ " = " ++ "i8 c" ++ s_
