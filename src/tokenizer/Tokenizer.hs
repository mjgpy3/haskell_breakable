module Tokenizer
  (BfToken(..),
  tokenize) where

import CharIdentifier
import Data.List

data BfToken =
  Identifier String |
  BfString String |
  BfInteger String |
  BfFloat String |
  KwdClass |
  KwdMeth |
  KwdModule |
  KwdNot |
  KwdAnd |
  KwdOr |
  KwdIf |
  KwdElse |
  KwdFor |
  KwdIn |
  KwdWhile |
  Newline |
  LeftCurl | RightCurl |
  LeftParen | RightParen |
  LeftSquare | RightSquare |
  Dot |
  Pipe |
  Comma |
  Lambda |
  Arrow |
  Plus |
  Minus |
  Times |
  Divide |
  Modulus |
  Assign |
  Equal | NotEqual |
  Lt | Gt |
  Leq | Geq
  deriving (Show, Eq)

tokenize :: String -> [BfToken]
tokenize [] = []
tokenize r@(x:xs)
  | isIgnored x         = tokenize xs
  | double /= Nothing   = [unjust double]
  | single /= Nothing   = (unjust single):tokenize xs
  | isStringDelimeter x = (BfString (fst stringSplit)):tokenize (drop 1 (snd stringSplit))
  | isNumeric x         = number:tokenize (snd numberSplit)
  | isIdentifier x      = (kwdOrIdent (x:(fst identSplit))):tokenize (snd identSplit)
  | otherwise           = error ("Unrecognized token: " ++ [x])
  where single = singleChar x
        double = doubleChar $ take 2 r
        identSplit = splitAt (whereEnds 0 isIdentifier xs) xs
        stringSplit = splitAt (whereStringEnds 0 ((/=) x) xs) xs
        numberSplit = splitAt (whereNumberEnds 0 isNumeric xs) xs
        joinedNumber = x:(fst numberSplit)
        number = if elem '.' joinedNumber then BfFloat joinedNumber else BfInteger joinedNumber
        hasNext = length xs > 0

unjust :: Maybe a -> a
unjust Nothing = error "unjust should never be called on Nothing"
unjust (Just a) = a

kwdOrIdent :: String -> BfToken
kwdOrIdent w =
  case w of
    "class" -> KwdClass
    "meth" -> KwdMeth
    "module" -> KwdModule
    "not" -> KwdNot
    "and" -> KwdAnd
    "or" -> KwdOr
    "if" -> KwdIf
    "else" -> KwdElse
    "for" -> KwdFor
    "in" -> KwdIn
    "while" -> KwdWhile
    _ -> Identifier w

whereStringEnds :: Int -> (Char -> Bool) -> String -> Int
whereStringEnds x _ [] = x
whereStringEnds x f (s:ss)
  | s == '\\' = whereEnds (x+2) f (tail ss)
  | f s       = whereEnds (x+1) f ss
  | otherwise = x

whereEnds :: Int -> (Char -> Bool) -> String -> Int
whereEnds x _ [] = x
whereEnds x f (s:ss)
  | f s       = whereEnds (x+1) f ss
  | otherwise = x

whereNumberEnds :: Int -> (Char -> Bool) -> String -> Int
whereNumberEnds x _ [] = x
whereNumberEnds x f (s:ss)
  | f s || s == '.' = whereNumberEnds (x+1) f ss
  | otherwise = x

doubleChar :: [Char] -> Maybe BfToken
doubleChar x =
  case x of
    "/=" -> Just NotEqual
    ":=" -> Just Assign
    "<=" -> Just Leq
    ">=" -> Just Geq
    "->" -> Just Arrow
    _ -> Nothing

singleChar :: Char -> Maybe BfToken
singleChar x =
  case x of
    '\n' -> Just Newline
    '(' -> Just LeftParen
    ')' -> Just RightParen
    '[' -> Just LeftSquare
    ']' -> Just RightSquare
    '{' -> Just LeftCurl
    '}' -> Just RightCurl
    '>' -> Just Gt
    '<' -> Just Lt
    '.' -> Just Dot
    '|' -> Just Pipe
    ',' -> Just Comma
    '\\' -> Just Lambda
    '+' -> Just Plus
    '-' -> Just Minus
    '*' -> Just Times
    '/' -> Just Divide
    '=' -> Just Equal
    '%' -> Just Modulus
    _ -> Nothing

main = print $ whereNumberEnds 0 isNumeric "42.5"
