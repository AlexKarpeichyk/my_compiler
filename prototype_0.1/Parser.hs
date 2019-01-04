-- Necessary Imports

import Lexer

-- Datatypes

data PROG = D DEC | PSEQ (DEC) (PROG) deriving (Show)
data DEC = DEF (String) (VARDEC) (BLOCK) deriving (Show)
data VARDEC = NULL | VARDECNE deriving (Show)
data VARDECNE = One (String) | Many (VARDECNE) (String) deriving (Show) 
data BLOCK = ENE deriving (Show, Eq)
data ENE = E | Seq (E) (ENE) deriving (Show)
data E = 
    ID String
  | STRING
  | Integer
  | BINOP 
  | COND (COMP) (BLOCK) (BLOCK)
  | SKIP
  | BLOCK
  | WHILE (COMP) (BLOCK)
  | REP (BLOCK) (COMP)
  | ASSIGN (String) (E) 
  | BREAK
  | CONTINUE deriving (Show, Eq)
data COMP = Eq (E) (E) | Less (E) (E) | Greater (E) (E) | LessEq (E) (E) | GreaterEq (E) (E) deriving (Show, Eq)
data BINOP = Add (E) (E) | Sub (E) (E) | Times (E) (E) | Divide (E) (E) deriving (Show, Eq)

-- Functions

parseBinop :: [Token] -> BINOP
parseBinop (h:f:t)
  | f == Plus = Add (getInt h) (getInt (head t))
  | otherwise = error "null"

getInt :: Token -> E
getInt (INT n) = n
