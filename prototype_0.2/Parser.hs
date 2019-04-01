module Parser
{-
( PROG (..)
, DEC (..)
, VARDEC (..)
, BLOCK (..)
, ENE (..)
, E (..)
, BLN (..)
, ARGS (..)
, COMP (..)
, BINOP (..)
, 
)-}
where

-- Necessary Imports

import Lexer

-- Datatypes

data PROG = P DEC | PSEQ (DEC) (PROG) deriving (Show)
data DEC = DEF (String) (VARDEC) (BLOCK) deriving (Show)
data VARDEC = None | One (String) | Many (String) (VARDEC) deriving (Show) 
data BLOCK = Block (ENE) deriving (Show, Eq)
data ENE = Single (E) | Seq (E) (ENE) deriving (Show, Eq)
data E = 
    ID (String)
  | STR (String)
  | INT (Integer)
  | BOOL (BLN)
  | BinOp (BINOP)
  | Comp (COMP) 
  | COND (COMP) (BLOCK) (BLOCK)
  | Nested (BLOCK)
  | WHILE (COMP) (BLOCK)
  | REP (BLOCK) (COMP)
  | ASSIGN (String) (E)
  | FUNCALL (String) (ARGS)
  | SKIP
  | BREAK
  | CONTINUE
  | PRINT (E) deriving (Show, Eq)
data BLN = T | F deriving (Show, Eq)
data ARGS = NoA | OneA (E) | ManyA (E) (ARGS) deriving (Show, Eq)
data COMP = Eq (E) (E) | Less (E) (E) | Greater (E) (E) | LessEq (E) (E) | GreaterEq (E) (E) deriving (Show, Eq)
data BINOP = Add (E) (E) | Sub (E) (E) | Mult (E) (E) | Div (E) (E) deriving (Show, Eq)  

-- Functions

-- 1. Parser pre-processing

isLBracket (LCurlyBracket x) = (True, x)
isLBracket _ = (False, -1)
isRBracket (RCurlyBracket x) = (True, x)
isRBracket _ = (False, -1)

getBlock x acc [] = acc
getBlock x acc (h:t)
  | not ((fst (isRBracket h)) && (snd (isRBracket h) == x)) = getBlock x (acc ++ [h]) t
  | otherwise = getBlock x acc []

getRest x [] = []
getRest x (h:t)
  | not ((fst (isRBracket h)) && (snd (isRBracket h) == x)) = getRest x t
  | otherwise = t

checkBrackets :: [Token] -> [Token] -> Bool
checkBrackets expec [] = null expec
checkBrackets expec (h:t)
  | not (elem h [LCurlyBracket 0, RCurlyBracket 0]) = checkBrackets expec t
  | h == LCurlyBracket 0 = checkBrackets (RCurlyBracket 0:expec) t
  | h == RCurlyBracket 0 && length expec /= 0 = (h == head expec) && checkBrackets (tail expec) t
  | otherwise = False

labelBrackets expec acc [] = []
labelBrackets expec acc (h:t)
  | h == LCurlyBracket 0 = (LCurlyBracket (acc)):(labelBrackets (acc:expec) (acc + 1) t)
  | h == RCurlyBracket 0 = (RCurlyBracket (head expec)):(labelBrackets (tail expec) acc t)
  | otherwise = h:(labelBrackets expec acc t)

getFuns acc _ [] = []
getFuns acc False (Def:t) = getFuns [] True t
getFuns acc True ((Bl x):t) = (Fun (acc ++ [Bl x])):(getFuns [] False t)
getFuns acc True (h:t) = getFuns (acc ++ [h]) True t
getFuns acc False (h:t) = error "Some error"

preProc [] = []
preProc (h:t)
  | not (fst (isLBracket h)) = h:(preProc t)
  | fst (isLBracket h) = Bl (preProc (getBlock (snd (isLBracket h)) [] t)):(preProc (getRest (snd (isLBracket h)) t))
  | otherwise = error "Tokens preprocessing error: unknown error."

preParse l
  | checkBrackets [] l = getFuns [] False (preProc (labelBrackets [] 1 l))
  | otherwise = error "Tokens preprocessing error: missmatched brackets in BLOCK formation."

-- 2. Main parser

parse [(Fun x)] = P (parseDec (Fun x))
parse ((Fun x):t) = PSEQ (parseDec (Fun x)) (parse t)
parse _ = error "Parse error: not a program." 

parseDec (Fun x) = DEF (getDefID x) (parseVardec (fst (getVardecAndBlock [] (tail x)))) (parseBlock (snd (getVardecAndBlock [] (tail x))))
parseDec _ = error "Parse error: error in function definition here."

getDefID ((IDENTIFIER x):t) = x
getDefID _ = error "Parse error: error in function definition."

getVardecAndBlock :: [Token] -> [Token] -> ([Token], Token)
getVardecAndBlock [] [LBracket,RBracket,EqualDefines,(Bl y)] = ([], (Bl y))
getVardecAndBlock acc (LBracket:(IDENTIFIER x):Comma:t) = getVardecAndBlock (acc ++ [IDENTIFIER x]) (LBracket:t)
getVardecAndBlock acc [LBracket,(IDENTIFIER x),RBracket,EqualDefines,(Bl y)] = ((acc ++ [IDENTIFIER x]), (Bl y))
getVardecAndBlock acc _ = error "Parse error: error in variable declaration."

parseVardec :: [Token] -> VARDEC
parseVardec [] = None
parseVardec [IDENTIFIER x] = One (x)
parseVardec ((IDENTIFIER x):t) = Many (x) (parseVardec t)
parseVardec _ = error "Parse error: error in variable declaration."

getBlock_ (h:f:t)
  | h /= EqualDefines = getBlock_ (f:t)
  | h == EqualDefines = f 

parseBlock :: Token -> BLOCK
parseBlock (Bl x) = Block (parseENE [] x)
parseBlock _ = error "Parse error: not a block."

parseENE :: [Token] -> [Token]-> ENE
parseENE acc [] = Single (parseE acc)
parseENE [] [x] = Single (parseE [x])
parseENE acc (h:Semicolon:t) = Seq (parseE (acc ++ [h])) (parseENE [] t)
parseENE acc (h:t) = parseENE (acc ++ [h]) t 

parseE :: [Token] -> E
parseE [BOOLEAN "true"] = BOOL T
parseE [BOOLEAN "false"] = BOOL F
parseE [IDENTIFIER s] = ID s
parseE [STRING s] = STR s
parseE [INTEGER x] = INT x
parseE [Skip] = SKIP
parseE [Bl x] = Nested (parseBlock (Bl x))
parseE [Break] = BREAK
parseE [Continue] = CONTINUE
parseE (Print:t) = PRINT (parsePrint [] t)
parseE ((IDENTIFIER s):Assign:t) = ASSIGN (s) (parseE t)
parseE [(IDENTIFIER s),LBracket,RBracket] = FUNCALL (s) (NoA)
parseE (IDENTIFIER s:LBracket:t) = FUNCALL (s) (parseArgs [] (LBracket:t))
parseE (If:t) = parseCond [] (If:t)
parseE (While:t) = parseWhile [] (If:t)
parseE (Repeat:(Bl y):Until:t)
  | elem LessThan t || elem GreaterThan t || elem LessEqual t || elem GreaterEqual t || elem Equal t = REP (parseBlock (Bl y)) (parseComp [] t)
  | otherwise = error "Parse error: error in repeat-loop."
parseE (Repeat:_) = error "Parse error: error in repeat-loop."
parseE l
  | elem LessThan l || elem GreaterThan l || elem LessEqual l || elem GreaterEqual l || elem Equal l = Comp (parseComp [] l) 
  | elem Plus l || elem Minus l = BinOp (parseBinop [] l)
  | elem Times l || elem Divide l = BinOp (parseBinop_ [] l)
  | otherwise = error "Parse error: some syntax error." 

parsePrint :: [Token] -> [Token] -> E
parsePrint acc [LBracket,RBracket] = PRINT (parseE acc)
parsePrint acc (LBracket:f:t) = parsePrint (acc ++ [f]) (LBracket:t)
parsePrint acc _ = error "Parse error: error in print statement."

parseArgs :: [Token] -> [Token] -> ARGS
parseArgs acc [LBracket,x,RBracket] = OneA (parseE (acc ++ [x]))
parseArgs acc (LBracket:x:Comma:t) = ManyA (parseE (acc ++ [x])) (parseArgs [] (LBracket:t))
parseArgs acc (LBracket:x:t) = parseArgs (acc ++ [x]) (LBracket:t)
parseArgs acc _ = error "Parse error: error in funciton call."

parseBinop :: [Token] -> [Token] -> BINOP
parseBinop acc [x]
  | elem x [Plus, Minus, Times, Divide] = error "Parse error: error in binary operation"
parseBinop acc (h:t) 
  | h == Plus = Add (parseE acc) (parseE t)
  | h == Minus = Sub (parseE acc) (parseE t)
  | otherwise = parseBinop (acc ++ [h]) t

parseBinop_ :: [Token] -> [Token] -> BINOP
parseBinop_ acc (h:t)
  | h == Times = Mult (parseE acc) (parseE t)
  | h == Divide = Div (parseE acc) (parseE t)
  | otherwise = parseBinop_ (acc ++ [h]) t

parseComp :: [Token] -> [Token] -> COMP
parseComp acc [x]
  | elem x [LessThan, GreaterThan, LessEqual, GreaterEqual, Equal] = error "Parse error: error in logical operation."
parseComp acc (h:t)
  | h == Equal = Eq (parseE acc) (parseE t)
  | h == LessThan = Less (parseE acc) (parseE t)
  | h == GreaterThan = Greater (parseE acc) (parseE t)
  | h == LessEqual = LessEq (parseE acc) (parseE t)
  | h == GreaterEqual = GreaterEq (parseE acc) (parseE t)
  | otherwise = parseComp (acc ++ [h]) t 

parseCond :: [Token] -> [Token] -> E
parseCond acc [If,x,Then,(Bl y),Else,(Bl z)]
  | elem LessThan l || elem GreaterThan l || elem LessEqual l || elem GreaterEqual l || elem Equal l = COND (parseComp [] (acc ++ [x])) (parseBlock (Bl y)) (parseBlock (Bl z))
  | otherwise = error "Parse error: error in conditional."
  where 
    l = (acc ++ [x])
parseCond acc (If:x:t) = parseCond (acc ++ [x]) (If:t)
parseCond acc _ = error "Parse error: error in conditional."

parseWhile :: [Token] -> [Token] -> E
parseWhile acc [While,x,Do,(Bl y)]
  | elem LessThan l || elem GreaterThan l || elem LessEqual l || elem GreaterEqual l || elem Equal l = WHILE (parseComp [] (acc ++ [x])) (parseBlock (Bl y))
  | otherwise = error "Parse error: error in while-lopp."
  where
    l = (acc ++ [x])
parseWhile acc (If:x:t) = parseWhile (acc ++ [x]) (While:t)
parseWhile acc _ = error "Parse error: error in while-lopp."
