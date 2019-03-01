-- Necessary Imports 

import Lexer

-- Datatypes

data PreBlock = Flat Token | Bl [PreBlock] deriving (Show) 

data BLOCK = Block (ENE) deriving (Show, Eq)
data ENE = Single (E) | Seq (E) (ENE) deriving (Show, Eq)
data E = INT_P Integer | Nest (BLOCK) | SKIP deriving (Show, Eq)

isInt (INT x) = True
isInt _ = False

seqLeft l (h:t)
  | h /= Semicolon = seqLeft (h:l) t
  | otherwise = l

seqRight (h:t)
  | h /= Semicolon = seqRight t
  | otherwise = t

parse :: [Token] -> [Token] -> BLOCK
parse expec (h:t)
--  | not (elem h [LCurlyBracket, RCurlyBracket]) = parseENE expec (h:t)
  | h == LCurlyBracket = Block (parseENE (RCurlyBracket:expec) t)
  | h == RCurlyBracket && length expec /= 0 = parse (tail expec) t
  | otherwise = error "Parse error: not a block."

parseENE :: [Token] -> [Token] -> ENE
parseENE expec (h:t)
  | elem Semicolon (h:t) = parseSeq expec (h:t)
  | otherwise = Single (parseE expec (h:t))

parseSeq :: [Token] -> [Token] -> ENE
parseSeq expec (h:t) = Seq (parseE expec (seqLeft [] (h:t))) (parseENE expec (seqRight (h:t)))

parseE :: [Token] -> [Token] -> E
parseE expec (h:t)
  | isInt h = parseInt h 
  | h == Skip = SKIP
  | otherwise = Nest (parse expec (h:t))

parseInt :: Token -> E
parseInt (INT x) = INT_P x
