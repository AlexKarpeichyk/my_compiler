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
  | BINOP 
  | COND (COMP) (E) (E)
  | Nested (BLOCK)
  | WHILE (COMP) (E)
  | REP (E) (COMP)
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

{-
getFuns [] = []
getFuns (Def:(IDENTIFIER x):LBracket:t) = [F (Def:(IDENTIFIER x):LBracket:(getFuns t))]
getFuns (RBracket:EqualDefines:(Bl x):t) = (RBracket:EqualDefines:[Bl x]) ++ (getFuns t) 
getFuns ((IDENTIFIER x):RBracket:EqualDefines:(Bl y):t) = ((IDENTIFIER x):RBracket:EqualDefines:[Bl y]) ++ (getFuns t)
getFuns ((IDENTIFIER x):Comma:(IDENTIFIER y):t) = ((IDENTIFIER x):Comma:(IDENTIFIER y):(getFuns t))
getFuns _ = error "Parse error: unknown smth"
-}

getFuns acc _ [] = []
getFuns acc False (Def:t) = getFuns [] True t
getFuns acc True ((Bl x):t) = (F (acc ++ [Bl x])):(getFuns [] False t)
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

seqLeft l (h:t)
  | h /= Semicolon = seqLeft (h:l) t
  | otherwise = l

seqRight (h:t)
  | h /= Semicolon = seqRight t
  | otherwise = t

{-
parse (h:t)
  | length (h:t) == 1 = P (parseDec (preParse (Def:t)))
  | length (h:t) > 1 = PSEQ (parseDec (preParse (Def:t))) (parse t)
parse (x:t) = parse t
-}

parse [(F x)] = P (parseDec (F x))
parse ((F x):t) = PSEQ (parseDec (F x)) (parse t)
parse _ = error "Parse error: not a program." 

{-
parseDec :: [Token] -> DEC 
parseDec (Def:(IDENTIFIER x):LBracket:t) = DEF (x) (parseVardec (getVardec [] t)) (parseBlock (getBlock_ t)) 
parseDec _ = error "Parse error: error in function definition."
-}

parseDec (F x) = DEF (getDefID x) (parseVardec (fst (getVardecAndBlock [] (tail x)))) (parseBlock (snd (getVardecAndBlock [] (tail x))))
parseDec _ = error "Parse error: error in function definition here."

getDefID ((IDENTIFIER x):t) = x
getDefID _ = error "Parse error: error in function definition."

getVardecAndBlock :: [Token] -> [Token] -> ([Token], Token)
--getVardecAndBlock acc [LBracket,(IDENTIFIER x),RBracket,EqualDefines,(Bl y)] = ([IDENTIFIER x], (Bl y))
getVardecAndBlock [] [LBracket,RBracket,EqualDefines,(Bl y)] = ([], (Bl y))
getVardecAndBlock acc (LBracket:(IDENTIFIER x):Comma:t) = getVardecAndBlock (acc ++ [IDENTIFIER x]) (LBracket:t)
getVardecAndBlock acc [LBracket,(IDENTIFIER x),RBracket,EqualDefines,(Bl y)] = ((acc ++ [IDENTIFIER x]), (Bl y))
getVardecAndBlock acc _ = error "Parse error: error in variable declaration."

{- 
getVardec acc (EqualDefines:t) = acc
getVardec acc (RBracket:EqualDefines:t) = acc
getVardec acc ((IDENTIFIER x):Comma:t) = getVardec (acc ++ [IDENTIFIER x]) t
getVardec acc ((IDENTIFIER x):RBracket:t) = getVardec (acc ++ [IDENTIFIER x]) t
getVardec acc _ = error "Parse error: error in function definition."
-}
parseVardec :: [Token] -> VARDEC
parseVardec [] = None
parseVardec [IDENTIFIER x] = One (x)
parseVardec ((IDENTIFIER x):t) = Many (x) (parseVardec t)
parseVardec _ = error "Parse error: error in variable declaration."

getBlock_ (h:f:t)
  | h /= EqualDefines = getBlock_ (f:t)
  | h == EqualDefines = f 

parseBlock :: Token -> BLOCK
parseBlock (Bl x) = Block (parseENE x)
parseBlock _ = error "Parse error: not a block."

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


