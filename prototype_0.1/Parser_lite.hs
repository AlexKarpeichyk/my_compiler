-- Necessary Imports 

import Lexer

-- Datatypes

data PreBlock = T (Token) | Bl [PreBlock] deriving (Show)

data BLOCK = Block (ENE) deriving (Show, Eq)
data ENE = Single (E) | Seq (E) (ENE) deriving (Show, Eq)
data E = INT_P Integer | Nest (BLOCK) | SKIP deriving (Show, Eq)

-- Functions 

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
  | h == LCurlyBracket 0 = Block (parseENE (RCurlyBracket 0:expec) t)
  | h == RCurlyBracket 0 && length expec /= 0 = parse (tail expec) t
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
