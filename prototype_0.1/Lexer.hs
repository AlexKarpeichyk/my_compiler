module Lexer
( Token (..)
--, tokenDict
, isNum
, toInt
, specNot
, clearSpaces
, lexx
--, findToken
, tokenize
) where

-- Necessary Imports

import Data.List.Split
import Data.Char
import Text.Regex.Posix

-- Datatypes

data Token = 
    Semicolon | LBracket | RBracket | LCurlyBracket | RCurlyBracket
  | EqualDefines | Equal | LessThan | GreaterThan | LessEq
  | GreaterEq | Comma | Assign | Plus | Minus | Mult | Div
  | ID String | INT Integer | BOOL String | STRING String | Def | Skip
  | If | Then | Else | While | Do | Repeat | Until 
  | Break | Continue deriving (Show, Eq)

--tokenDict :: [(Token, String)]
--tokenDict = [(Semicolon, ";"), (LBracket, "("), (RBracket, ")")]

-- Functions 

isID :: [Char] -> Bool
isID [] = False
isID [x]
  | isLower x = True
  | otherwise = False
isID (h:t)
  | isLower h && inRegex t = True
  | otherwise = False

inRegex :: String -> Bool
inRegex s = (s =~ "[a-z]*[A-Z]*[0-9]*_*" :: Bool)

isString :: String -> Bool
isString s
  | head s == '"' && last s == '"' = True
  | otherwise = False

isNum :: [Char] -> Bool
isNum [] = True
isNum [x] = isDigit x
isNum (h:t)
  | isDigit h = isNum t
  | otherwise = False

toInt :: String -> Integer
toInt s = read s :: Integer

specNot :: [String] -> [String]
specNot [] = []
specNot [x] = [x]
specNot (h:f:t)
  | h == "<" && f == "=" = "<=":specNot t
  | h == ">" && f == "=" = ">=":specNot t
  | h == "=" && f == "=" = "==":specNot t
  | h == ":" && f == "=" = ":=":specNot t
  | otherwise = h:(specNot (f:t))

{-
fixStrings :: [String] -> [String]
fixStrings [] = []
fixStrings [x] = [x]
fixStrings (h:t)
  | head h == '"' && last h == '"' = h:(fixStrings t)
  | otherwise = fixStrings ((combineStrings h (head t)):(tail t))
-}

combineStrings :: String -> String -> String
combineStrings l r = l ++ " " ++ r
 
clearSpaces :: [String] -> [String]
clearSpaces l = filter (\x -> x /= "" && x /= " ") l

lexx :: String -> [Token]
lexx s = tokenize (specNot (clearSpaces (split (oneOf " \t\n(){}=+-/*><;,:") s))) []

{-
findToken :: String -> [(Token, String)] -> Token 
findToken tok (h:t)
  | tok == snd h = fst h
  | otherwise = findToken tok t 
-}

tokenize :: [String] -> [Token] -> [Token]
tokenize [] l = l
--tokenize (h:t) l = tokenize t (l ++ [findToken h tokenDict])  
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
  | (isString h) = tokenize (t) (l ++ [STRING h])
  | (isID h) = tokenize (t) (l ++ [ID h])
  | otherwise = error ("token not in language: " ++ "\"" ++ h ++ "\"")
