import Lexer
import Parser

data LLVM = LLVM_IR ()

printf :: String
printf = "declare i32 @printf(i8*, ...)"

intPrint :: String
intPrint = "@.IntegerPrint = private constant [4 x i8] c\"%d\\0A\\00\""

codegen (Prog p) = (codegenGlobals 1 p) ++ intPrint ++ "\n\n" ++ printf ++ (codegenProg (memory p) 0 p)

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
memory [] = []
memory ((DEF s p (Block x)):t) = (s, memory_ x):(memory t)

memory_ :: [E] -> [(String, TYPE)]
memory_ [] = []
memory_ ((ASSIGN s (BinOp b)):t) = (s, Int_):(memory_ t)
memory_ ((ASSIGN s (INT x)):t) = (s, Int_):(memory_ t)
memory_ ((ASSIGN s (STR x)):t) = (s, String_):(memory_ t)
memory_ ((ASSIGN s (BOOL x)):t) = (s, Bool_):(memory_ t)
memory_ ((COND c (Block x) (Block y)):t) = (memory_ x) ++ (memory_ y) ++ (memory_ t)
memory_ ((WHILE c (Block x)):t) = (memory_ x) ++ (memory_ t)
memory_ (h:t) = memory_ t

codegenProg :: [(String, [(String, TYPE)])] -> Integer -> [DEC] -> String
codegenProg mem count [] = ""
codegenProg mem count (h:t) = (fst (codegenDec (head mem) count h)) ++ (codegenProg (tail mem) (snd (codegenDec (head mem) count h)) t)

codegenDec :: (String, [(String, TYPE)]) -> Integer -> DEC -> (String, Integer)
codegenDec mem count (DEF s p b)
  | s == "main" = (("\n\n" ++ "define i32 @" ++ s ++ "(" ++ (codegenParams p) ++ ")" ++ "\n" ++ "{" ++ (fst (codegenBlock (snd mem) count b)) ++ "\n\t" ++ "ret i32 0" ++ "\n" ++ "}"), snd (codegenBlock (snd mem) count b))
  | otherwise = (("\n\n" ++ "define void @" ++ s ++ "(" ++ (codegenParams p) ++ ")" ++ "\n" ++ "{" ++ (fst (codegenBlock (snd mem) count b)) ++ "\n\t" ++ "ret void" ++ "\n" ++ "}"), snd (codegenBlock (snd mem) count b))

codegenParams :: [PARAM] -> String
codegenParams [] = ""
codegenParams [Param Int_ s] = "i32 %" ++ s
codegenParams [Param String_ s] = "i8* %" ++ s
codegenParams [Param Bool_ s] = "i1 %" ++ s
codegenParams (h:t) = (codegenParams [h]) ++ ", " ++ (codegenParams t)

codegenBlock :: [(String, TYPE)] -> Integer -> BLOCK -> (String, Integer)
codegenBlock mem count (Block []) = ("", count)
codegenBlock mem count (Block exps) = (((fst (codegenExps mem count exps)) ++ "\n\t"), snd (codegenExps mem count exps))

codegenExps :: [(String, TYPE)] -> Integer -> [E] -> (String, Integer)
codegenExps mem count [] = ("", count)
codegenExps mem count (h:t) = ("\n\t" ++ (fst (codegenE mem count h)) ++ (fst (codegenExps mem (snd (codegenE mem count h)) t)), snd (codegenExps mem (snd (codegenE mem count h)) t))

codegenE mem count (ASSIGN s (INT x)) = (("%" ++ s ++ " = alloca i32, align 4" ++ "\n\t" ++ "store i32 " ++ (show x) ++ ", i32* %" ++ s ++ ", align 4"), count)
codegenE mem count (ASSIGN s (STR x)) = (("%" ++ s ++ " = getelementptr [" ++ (show (length x)) ++ " x i8],[" ++ (show (length x)) ++ " x i8]* @." ++ s ++ ", i32 0, i32 0"), count)
codegenE mem count (ASSIGN s (BOOL x))
  | x == T = (("%" ++ s ++ " = alloca i1, align 4" ++ "\n\t" ++ "store i1 1, i1* %" ++ s ++ ", align 4" ++ "\n\t" ++ "%" ++ (show (count + 1)) ++ " = load i1, i1* %" ++ s ++ ", align 4"), (count + 1))
  | x == F = (("%" ++ s ++ " = alloca i1, align 4" ++ "\n\t" ++ "store i1 0, i1* %" ++ s ++ ", align 4" ++ "\n\t" ++ "%" ++ (show (count + 1)) ++ " = load i1, i1* %" ++ s ++ ", align 4"), (count + 1))
codegenE mem count (PRINT (ID s))
  | (findType mem s) == String_ = (("call i32 (i8*, ...) @printf(i8* %" ++ s ++ ")"), count)
  | (findType mem s) == Int_ = (("%" ++ (show (count + 1)) ++ " = load i32, i32* %" ++ s ++ ", align 4" ++ "\n\t" ++ "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.IntegerPrint, i32 0, i32 0), i32 %" ++ (show (count + 1)) ++ ")"), (count + 1))
codegenE mem count _ = error "not yet pal"

findType :: [(String, TYPE)] -> String -> TYPE
findType (h:t) s
  | (fst h) == s = snd h
  | otherwise = findType t s
                                                                                                                                                                                          83,1          Bot

