import Lexer

data PROG = D DEC | PSEQ (DEC) (PROG) deriving (Show)
data DEC = DEF (String) (VARDEC) (BLOCK) deriving (Show)
data VARDEC = NULL | VARDECNE deriving (Show)
data VARDECNE = One (String) | Many (VARDECNE) (String) deriving (Show) 
data BLOCK = ENE deriving (Show, Eq)
data ENE = E | Seq (E) (ENE) deriving (Show)
data E = 
    Id String
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

parseBinop :: [Token] -> Binop
parseBinop (h:f:t)
  | f == Lexer.Plus = Add h (parseBinop t)
  | f == Lexer.Minus = Sub h (parseBinop t)
  | f == Lexer.Mult = Times h (parseBinop t)
  | f == Lexer.Div = Divide h (parseBinop t)
  | otherwise = error "syntax error"
