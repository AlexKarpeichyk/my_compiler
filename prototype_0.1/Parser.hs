-- Necessary Imports

import Lexer

-- Datatypes

--data PROG = D DEC | PSEQ (DEC) (PROG) deriving (Show)
--data DEC = DEF (String) (VARDEC) (BLOCK) deriving (Show)
--data VARDEC = NULL | VARDECNE deriving (Show)
--data VARDECNE = One (String) | Many (VARDECNE) (String) deriving (Show) 
data BLOCK = ENE deriving (Show, Eq)
data ENE = E deriving (Show, Eq)
-- | Seq (E) (ENE) deriving (Show)
data E = 
--    ID String
--  | STRING
    INT_P (Integer)
--  | BINOP 
--  | COND (COMP) (BLOCK) (BLOCK)
  | SKIP deriving (Show, Eq)
-- | BLOCK
-- | WHILE (COMP) (BLOCK)
-- | REP (BLOCK) (COMP)
-- | ASSIGN (String) (E) 
-- | BREAK
-- | CONTINUE deriving (Show, Eq)
--data COMP = Eq (E) (E) | Less (E) (E) | Greater (E) (E) | LessEq (E) (E) | GreaterEq (E) (E) deriving (Show, Eq)
--data BINOP = Add (E) (E) | Sub (E) (E) | Times (E) (E) | Divide (E) (E) deriving (Show, Eq)

-- Functions

isInt (INT x) = True
isInt _ = False

parse (h:t)
  | h == LCurlyBracket = parseE t
  | otherwise = error "Token not valid"

parseE :: [Token] -> E
parseE (h:t)   
  | isInt h = parseInt h
  | otherwise = parseSkip h

parseENE (h:t) = parseE (h:t)

parseInt :: Token -> E
parseInt (INT x) = INT_P x

parseSkip Skip = SKIP
