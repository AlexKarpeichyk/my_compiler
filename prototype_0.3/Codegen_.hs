import Lexer 
import Parser

data LLVM = LLVM_IR () 

printf :: String
printf = "declare i32 @printf(i8*, ...)"

intPrint :: String
intPrint = "@.IntegerPrint = private constant [4 x i8] c\"%d\\0A\\00\""

codegen (Prog p) = (codegenGlobals 1 p) ++ intPrint ++ "\n\n" ++ printf ++ (codegenProg (memory p) 1 p)

codegenGlobals count [] = ""
codegenGlobals count ((DEF s p (Block x)):t) = (fst (searchStrings count x)) ++ (codegenGlobals (snd (searchStrings count x)) t)

searchStrings count [] = ("", count)
searchStrings count ((STR s):t) = (("@.str." ++ (show count) ++ " = private constant [" ++ (show (length s)) ++ " x i8] c" ++ stringIR s ++ "\n" ++ (fst (searchStrings (count + 1) t))), snd (searchStrings (count + 1) t))
--searchStrings count ((INT x):t) = (("@.str." ++ (show count) ++ " = private constant [" ++ (show (length s)) ++ " x i8] c" ++ stringIR s ++ "\n" ++ (fst (searchStrings (count + 1) t))), snd (searchStrings (count + 1) t))
searchStrings count ((FUNCALL s e):t) = ((fst (searchStrings count e)) ++ (fst (searchStrings (snd (searchStrings count e)) t)), snd (searchStrings (snd (searchStrings count e)) t))
searchStrings count ((PRINT (STR s)):t) = ((fst (searchStrings count [STR s])) ++ (fst (searchStrings (snd (searchStrings count [STR s])) t)), snd (searchStrings (snd (searchStrings count [STR s])) t))
searchStrings count ((ASSIGN x (STR s)):t) = (("@." ++ x ++ " = private constant [" ++ (show (length s)) ++ " x i8] c" ++ stringIR s ++ "\n" ++ (fst (searchStrings count t))), snd (searchStrings count t))
searchStrings count ((COND c (Block a) (Block b)):t) = ((fst (searchStrings count a)) ++ (fst (searchStrings (snd (searchStrings count a)) b)) ++ (fst (searchStrings (snd (searchStrings (snd (searchStrings count a)) b)) t)), snd (searchStrings (snd (searchStrings (snd (searchStrings count a)) b)) t))
searchStrings count ((WHILE c (Block a)):t) = ((fst (searchStrings count a)) ++ (fst (searchStrings (snd (searchStrings count a)) t)), snd (searchStrings (snd (searchStrings count a)) t))
--searchStrings count ((COND c (Block a) (Block b))) = ((fst (searchStrings count [c])) ++ (fst (searchStrings (snd (searchStrings count [c])) a)) ++ (fst (searchStrings (snd (searchStrings (snd (searchStrings count [c])) a)) b)), snd (searchStrings (snd (searchStrings (snd (searchStrings count [c])) a)) b))
--searchStrings count [Eq l r] = ((fst (searchStrings count [l])) ++ (fst (searchStrings (snd (searchStrings count [l])) [r])), snd (searchStrings (snd (searchStrings count [l])) [r]))
searchStrings count (h:t) = (searchStrings count t)

stringIR [x, '"'] = x:"\\0A\\00\""
stringIR (h:t) = h:(stringIR t)

memory :: [DEC] -> [(String, [(String, TYPE)])]

