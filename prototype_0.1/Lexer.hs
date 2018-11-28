module Lexer
( Token (..)
, isNum
, toInt
, fin
, clearSpaces
, lexx
, tokenize
) where

-- Necessary Imports

import Data.List.Split
import Data.Char

-- Datatypes

data Token = Semicolon
  | LBracket
  | RBracket
  | LCurlyBracket
  | RCurlyBracket
  | EqualDefines
  | Equal
  | LessThan 
  | GreaterThan
  | LessEq
  | GreaterEq
  | Comma
  | Assign
  | Plus
  | Minus
  | Mult
  | Div
  | ID String
  | INT Integer
  | BOOL String
  | Def
  | Skip
  | If
  | Then
  | Else
  | While
  | Do
  | Repeat
  | Until
  | Break 
  | Continue deriving (Show, Eq)

-- Functions 

isID :: [Char] -> Bool
isID s = isLower (head s)

isNum :: [Char] -> Bool
isNum [] = error "no argument found"
isNum [x] = isDigit x
isNum (h:t)
  | isDigit h = isNum t
  | otherwise = False

toInt :: String -> Integer
toInt s = read s :: Integer

fin :: [String] -> [String]
fin [] = []
fin [x] = [x]
fin (h:f:t)
  | h == "<" && f == "=" = "<=":fin t
  | h == ">" && f == "=" = ">=":fin t
  | h == "=" && f == "=" = "==":fin t
  | h == ":" && f == "=" = ":=":fin t
  | otherwise = h:(fin (f:t))

clearSpaces :: [String] -> [String]
clearSpaces l = filter (\x -> x /= "" && x /= " ") l

lexx :: String -> [Token]
lexx s = tokenize (fin (clearSpaces (split (oneOf " (){}=+-/*><;,:") s))) []

tokenize :: [String] -> [Token] -> [Token]
tokenize [] l = l
tokenize (h:t) l
  | h == ";" = tokenize (t) (l ++ [Semicolon])
  | h == "(" = tokenize (t) (l ++ [LBracket])
  | h == ")" = tokenize (t) (l ++ [RBracket])
  | h == "{" = tokenize (t) (l ++ [LCurlyBracket])
  | h == "}" = tokenize (t) (l ++ [RCurlyBracket])
  | h == "=" = tokenize (t) (l ++ [EqualDefines])
  | h == "==" = tokenize (t) (l ++ [Equal])
  | h == "<" = tokenize (t) (l ++ [LessThan])
  | h == ">" = tokenize (t) (l ++ [GreaterThan])
  | h == "<=" = tokenize (t) (l ++ [LessEq])
  | h == ">=" = tokenize (t) (l ++ [GreaterEq])
  | h == "," = tokenize (t) (l ++ [Comma])
  | h == ":=" = tokenize (t) (l ++ [Assign])
  | h == "+" = tokenize (t) (l ++ [Plus])
  | h == "-" = tokenize (t) (l ++ [Minus])
  | h == "*" = tokenize (t) (l ++ [Mult])
  | h == "/" = tokenize (t) (l ++ [Div])
  | h == "def" = tokenize (t) (l ++ [Def])
  | h == "skip" = tokenize (t) (l ++ [Skip])
  | h == "if" = tokenize (t) (l ++ [If])
  | h == "then" = tokenize (t) (l ++ [Then])
  | h == "else" = tokenize (t) (l ++ [Else])
  | h == "while" = tokenize (t) (l ++ [While])
  | h == "do" = tokenize (t) (l ++ [Do])
  | h == "repeat" = tokenize (t) (l ++ [Repeat])
  | h == "until" = tokenize (t) (l ++ [Until])
  | h == "break" = tokenize (t) (l ++ [Break])
  | h == "continue" = tokenize (t) (l ++ [Continue])
  | h == "true" = tokenize (t) (l ++ [(BOOL "true")])
  | h == "false" = tokenize (t) (l ++ [(BOOL "false")])
  | (isNum h) = tokenize (t) (l ++ [INT (toInt h)])
  | (isID h) = tokenize (t) (l ++ [ID h])
  | otherwise = error ("token not in language: " ++ "\"" ++ h ++ "\"")

