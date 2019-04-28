import Lexer
import Parser

printf :: String
printf = "declare i32 @printf(i8*, ...)"

intPrint :: String
intPrint = "@.IntegerPrint = private constant [4 x i8] c\"%d\\0A\\00\""

truePrint :: String
truePrint = "\n" ++ "@True = private constant [6 x i8] c\"true\\0A\\00\""

falsePrint :: String
falsePrint = "\n" ++ "@False = private constant [7 x i8] c\"false\\0A\\00\""

boolPrint :: String 
boolPrint = "define void @bool_print(i1 %x)" ++ "\n" ++ "{" ++ "\n" ++ "entry:" ++ "\n\t" ++ "%bl = icmp eq i1 %x, 1" ++ "\n\t" ++ "br i1 %bl, label %truePrint, label %falsePrint" ++ "\n" ++ "falsePrint:" ++ "\n\t" ++ "%False = getelementptr [7 x i8], [7 x i8]* @False, i32 0, i32 0" ++ "\n\t" ++ "call i32 (i8*, ...) @printf(i8* %False)" ++ "\n\t" ++ "br label %cont" ++ "\n" ++ "truePrint:" ++ "\n\t" ++ "%True = getelementptr [6 x i8], [6 x i8]* @True, i32 0, i32 0" ++ "\n\t" ++ "call i32 (i8*, ...) @printf(i8* %True)" ++ "\n\t" ++ "br label %cont" ++ "\n" ++ "cont:" ++ "\n\t" ++ "ret void" ++ "\n" ++ "}"


codegen (Prog p) = (codegenGlobals 1 p) ++ intPrint ++ truePrint ++ falsePrint ++ "\n\n" ++ printf ++ "\n\n" ++ boolPrint ++ "\n\n" ++ (codegenProg (memory p) 0 p)

codegenGlobals count [] = ""
codegenGlobals count ((DEF s p (Block x)):t) = (fst (searchStrings count x)) ++ (codegenGlobals (snd (searchStrings count x)) t)

searchStrings count [] = ("", count)
searchStrings count ((STR s):t) = (("@.str." ++ (show count) ++ " = private constant [" ++ (show (length s)) ++ " x i8] c" ++ stringIR s ++ "\n" ++ (fst (searchStrings (count + 1) t))), snd (searchStrings (count + 1) t))
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
memory ((DEF s p (Block x)):t) = (s, (memoryP p) ++ (memory_ x)):(memory t)

memoryP :: [PARAM] -> [(String, TYPE)]
memoryP [] = []
memoryP ((Param type_ s):t) = (s, type_):(memoryP t)

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


codegenE mem count (ASSIGN s (INT x)) = (("%" ++ (show (count + 1)) ++ " = alloca i32, align 4" ++ "\n\t" ++ "store i32 " ++ (show x) ++ ", i32* %" ++ (show (count + 1)) ++ ", align 4" ++ "\n\t" ++ "%" ++ s ++ " = load i32, i32* %" ++ (show (count + 1)) ++ ", align 4"), (count + 1))
codegenE mem count (ASSIGN s (STR x)) = (("%" ++ s ++ " = getelementptr [" ++ (show (length x)) ++ " x i8],[" ++ (show (length x)) ++ " x i8]* @." ++ s ++ ", i32 0, i32 0"), count)
codegenE mem count (ASSIGN s (BOOL x))
  | x == T = (("%" ++ (show (count + 1)) ++ " = alloca i1, align 4" ++ "\n\t" ++ "store i1 1, i1* %" ++ (show (count + 1)) ++ ", align 4" ++ "\n\t" ++ "%" ++ s ++ " = load i1, i1* %" ++ (show (count + 1)) ++ ", align 4"), (count + 1))
  | x == F = (("%" ++ (show (count + 1)) ++ " = alloca i1, align 4" ++ "\n\t" ++ "store i1 0, i1* %" ++ (show (count + 1)) ++ ", align 4" ++ "\n\t" ++ "%" ++ s ++ " = load i1, i1* %" ++ (show (count + 1)) ++ ", align 4"), (count + 1))
codegenE mem count (ASSIGN s (BinOp bo)) = (("%" ++ (show (count + 1)) ++ " = alloca i32, align 4" ++ (fst (codegenBinop (count + 1) bo)) ++ "\n\t" ++ "store i32 %" ++ (show (snd (codegenBinop (count + 1) bo))) ++ ", i32* %" ++ (show (count + 1)) ++ ", align 4" ++ "\n\t" ++ "%" ++ s ++ " = load i32, i32* %" ++ (show (count + 1)) ++ ", align 4"), snd (codegenBinop (count + 1) bo))
codegenE mem count (PRINT (ID s))
  | (findType mem s) == String_ = (("call i32 (i8*, ...) @printf(i8* %" ++ s ++ ")"), count)
  | (findType mem s) == Int_ = (("call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.IntegerPrint, i32 0, i32 0), i32 %" ++ s ++ ")"), count)
  | (findType mem s) == Bool_ = (("call void @bool_print(i1 " ++ s ++ ")"), count)
codegenE mem count (FUNCALL s e) = (("call void @" ++ s ++ "(" ++ (codegenArgs mem e) ++ ")"), count)
codegenE mem count _ = error "not yet pal"

findType :: [(String, TYPE)] -> String -> TYPE
findType (h:t) s
  | (fst h) == s = snd h
  | otherwise = findType t s

codegenArgs mem [] = ""
codegenArgs mem [ID s]
  | (findType mem s) == String_ = "i8* %" ++ s
  | (findType mem s) == Int_ = "i32 %" ++ s
  | (findType mem s) == Bool_ = "i1 %" ++ s
codegenArgs mem ((ID s):t) = (codegenArgs mem [ID s]) ++ ", " ++ (codegenArgs mem t)
codegenArgs mem _ = error "Invalid argument"

--codegenBinop count (Mult l (BinOp r)) = ("\n\t" ++ (fst (codegenBinop count r)) ++ "\n\t" ++ "%" ++ (show (snd (codegenBinop count r) + 1)) ++ " = mul i32 " ++ (codegenBinop_ l) ++ ", %" ++ (show (snd (codegenBinop count r))), snd (codegenBinop count r) + 1)
--codegenBinop count (Mult l r) = (("\n\t" ++ "%" ++ (show (count + 1)) ++ " = mul i32 " ++ (codegenBinop_ l) ++ ", " ++ (codegenBinop_ r)), (count + 1))
--codegenBinop count (Mult (BinOp l) r) = ("\n\t" ++ (fst (codegenBinop count l)) ++ "\n\t" ++ "%" ++ (show (snd (codegenBinop count l) + 1)) ++ " = mul i32 " ++ (codegenBinop_ r) ++ ", " ++ (show (snd (codegenBinop count l))), snd (codegenBinop count l) + 1)
--codegenBinop count (Mult (BinOp l) (BinOp r)) = ((fst (codegenBinop count l)) ++ (fst (codegenBinop (snd (codegenBinop count l)) r)), snd (codegenBinop (snd (codegenBinop count l)) r))

-- Codegen - Addition

codegenBinop count (Add (BinOp l) (BinOp r)) = (((fst (codegenBinop count l)) ++ (fst (codegenBinop (snd (codegenBinop count l)) r)) ++ "\n\t" ++ "%" ++ (show ((snd (codegenBinop (snd (codegenBinop count l)) r)) + 1)) ++ " = add i32 %" ++ (show (snd (codegenBinop count l))) ++ ", %" ++ (show (snd (codegenBinop (snd (codegenBinop count l)) r)))), snd (codegenBinop (snd (codegenBinop count l)) r) + 1)
codegenBinop count (Add l (BinOp r)) = (((fst (codegenBinop count r)) ++ "\n\t" ++ "%" ++ (show (snd (codegenBinop count r) + 1)) ++ " = add i32 " ++ (codegenBinop_ l) ++ ", %" ++ (show (snd (codegenBinop count r)))), snd (codegenBinop count r) + 1)
codegenBinop count (Add (BinOp l) r) = (((fst (codegenBinop count l)) ++ "\n\t" ++ "%" ++ (show (snd (codegenBinop count l) + 1)) ++ " = add i32 " ++ (codegenBinop_ r) ++ ", %" ++ (show (snd (codegenBinop count l)))), snd (codegenBinop count l) + 1)
codegenBinop count (Add l r) = (("\n\t" ++ "%" ++ (show (count + 1)) ++ " = add i32 " ++ (codegenBinop_ l) ++ ", " ++ (codegenBinop_ r)), (count + 1))

-- Codegen - Subtraction

codegenBinop count (Sub (BinOp l) (BinOp r)) = (((fst (codegenBinop count l)) ++ (fst (codegenBinop (snd (codegenBinop count l)) r)) ++ "\n\t" ++ "%" ++ (show ((snd (codegenBinop (snd (codegenBinop count l)) r)) + 1)) ++ " = add i32 %" ++ (show (snd (codegenBinop count l))) ++ ", %" ++ (show (snd (codegenBinop (snd (codegenBinop count l)) r)))), snd (codegenBinop (snd (codegenBinop count l)) r) + 1)
codegenBinop count (Sub l (BinOp r)) = (((fst (codegenBinop count r)) ++ "\n\t" ++ "%" ++ (show (snd (codegenBinop count r) + 1)) ++ " = sub i32 " ++ (codegenBinop_ l) ++ ", %" ++ (show (snd (codegenBinop count r)))), snd (codegenBinop count r) + 1)
codegenBinop count (Sub (BinOp l) r) = (((fst (codegenBinop count l)) ++ "\n\t" ++ "%" ++ (show (snd (codegenBinop count l) + 1)) ++ " = sub i32 %" ++ (show (snd (codegenBinop count l))) ++ ", " ++ (codegenBinop_ r)), snd (codegenBinop count l) + 1)
codegenBinop count (Sub l r) = (("\n\t" ++ "%" ++ (show (count + 1)) ++ " = sub i32 " ++ (codegenBinop_ l) ++ ", " ++ (codegenBinop_ r)), (count + 1))

-- Codegen - Division

codegenBinop count (Div l (BinOp r)) = ("\n\t" ++ (fst (codegenBinop count r)) ++ "%" ++ (show (snd (codegenBinop count r) + 1)) ++ " = sdiv i32 " ++ (codegenBinop_ l) ++ ", %" ++ (show (snd (codegenBinop count r))), snd (codegenBinop count r) + 1)
codegenBinop count (Div l r) = (("\n\t" ++ "%" ++ (show (count + 1)) ++ " = sdiv i32 " ++ (codegenBinop_ l) ++ ", " ++ (codegenBinop_ r)), (count + 1))

-- Codegen - Multiplication

codegenBinop count (Mult l (BinOp r)) = ("\n\t" ++ (fst (codegenBinop count r)) ++ "%" ++ (show (snd (codegenBinop count r) + 1)) ++ " = mul i32 " ++ (codegenBinop_ l) ++ ", %" ++ (show (snd (codegenBinop count r))), snd (codegenBinop count r) + 1)
codegenBinop count (Mult l r) = (("\n\t" ++ "%" ++ (show (count + 1)) ++ " = mul i32 " ++ (codegenBinop_ l) ++ ", " ++ (codegenBinop_ r)), (count + 1))

codegenBinop_ (ID s) = "%" ++ s
codegenBinop_ (INT x) = show x
codegenBinop_ _ = error "Not an integer"

