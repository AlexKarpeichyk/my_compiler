-- Necessary Imports

import Lexer

-- Datatypes

data PROG = P DEC | PSEQ (DEC) (PROG) deriving (Show)
data DEC = DEF (String) (VARDEC) (BLOCK) deriving (Show)
data VARDEC = NULL | VARDECNE deriving (Show)
data VARDECNE = One (String) | Many (VARDECNE) (String) deriving (Show) 
data BLOCK = Block (ENE) deriving (Show, Eq)
data ENE = Single (E) | Seq (E) (ENE) deriving (Show, Eq)
data E = 
    ID (String)
  | STR (String)
  | INT (Integer)
  | BINOP 
  | COND (COMP) (BLOCK) (BLOCK)
  | Nested (BLOCK)
  | WHILE (COMP) (BLOCK)
  | REP (BLOCK) (COMP)
  | ASSIGN (String) (E)
  | FUNCALL (String) (ARGS)
  | SKIP
  | BREAK
  | CONTINUE deriving (Show, Eq)
data ARGS = Nil | ARGSNE deriving (Show, Eq)
data ARGSNE = OneA (E) | ManyA (ARGSNE) deriving (Show, Eq)
data COMP = Eq (E) (E) | Less (E) (E) | Greater (E) (E) | LessEq (E) (E) | GreaterEq (E) (E) deriving (Show, Eq)
data BINOP = Add (E) (E) | Sub (E) (E) | Times (E) (E) | Divide (E) (E) deriving (Show, Eq)

-- Functions

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

preProc [] = []
preProc (h:t)
  | not (fst (isLBracket h)) = h:(preProc t)
  | fst (isLBracket h) = Bl (preProc (getBlock (snd (isLBracket h)) [] t)):(preProc (getRest (snd (isLBracket h)) t))
  | otherwise = error "Tokens preprocessing error: unknown error."

preParse l
  | checkBrackets [] l = preProc (labelBrackets [] 1 l)
  | otherwise = error "Tokens preprocessing error: missmatched brackets in BLOCK formation."

seqLeft l (h:t)
  | h /= Semicolon = seqLeft (h:l) t
  | otherwise = l

seqRight (h:t)
  | h /= Semicolon = seqRight t
  | otherwise = t

--parse (h:t) = parseBlock (last (preParse (h:t)))

parse (h:t)
  | h == Def = parseDec (h:t)

parseBlock :: Token -> BLOCK
parseBlock (Bl x) = Block (parseENE x)

parseENE :: [Token] -> ENE
parseENE [x] = Single (parseE [x])
parseENE (h:t)
  | elem Semicolon (h:t) = parseSeq (h:t)
  | otherwise = error "Parse error: expression sequence incomplete (missing ';')."

parseSeq :: [Token] -> ENE
parseSeq (h:t) = Seq (parseE (seqLeft [] (h:t))) (parseENE (seqRight (h:t)))

parseE :: [Token] -> E
parseE [IDENTIFIER s] = ID s
parseE [STRING s] = STR s
parseE [INTEGER x] = INT x
parseE [Skip] = SKIP
parseE [Bl x] = Nested (parseBlock (Bl x))
parseE [Break] = BREAK
parseE [Continue] = CONTINUE
parseE [x] = error "Parse error: token not yet supported."

