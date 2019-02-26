-- Necessary Imports 

import Lexer

-- Datatypes

data BLOCK = Block (ENE) deriving (Show)
data ENE = E | Seq (E) (ENE) derivign (Show)
data E = INT_P Integer | BLOCK | SKIP deriving (Show, Eq)

isInt (INT x) = True
isInt _ = False

parseInt :: Token -> E
parseInt (INT x) = INT_P x

parseBlock (LCurlyBracket:t)
  |  
