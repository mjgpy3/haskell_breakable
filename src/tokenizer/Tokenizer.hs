module Tokenizer
  (BfToken(..),
  tokenize) where

import CharIdentifier
import Data.List

data BfToken = 
  Identifier String |
  BfString String |
  BfInteger String |
  KwdClass |
  KwdMeth |
  KwdModule |
  KwdNot |
  KwdAnd |
  KwdOr |
  KwdIf |
  KwdFor |
  KwdIn |
  KwdWhile |
  Newline |
  LeftCurl | RightCurl |
  LeftParen | RightParen |
  LeftSquare | RightSquare |
  Dot |
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
tokenize (x:xs)
  | isIgnored x         = tokenize xs
  | double /= Nothing   = [unjust double]
  | single /= Nothing   = (unjust single):tokenize xs
  | isStringDelimeter x = (BfString (fst stringSplit)):tokenize (drop 1 (snd stringSplit))
  | isNumeric x         = (BfInteger (x:(fst numberSplit))):tokenize (snd numberSplit)
  | isIdentifier x      = (kwdOrIdent (x:(fst identSplit))):tokenize (snd identSplit)
  | otherwise           = error ("Unrecognized token: " ++ [x])
  where single = singleChar x
        double = doubleChar $ take 2 (x:xs)
        identSplit = splitAt (whereEnds 0 isIdentifier xs) xs
        stringSplit = splitAt (whereEnds 0 ((/=) x) xs) xs
        numberSplit = splitAt (whereEnds 0 isNumeric xs) xs
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
    "for" -> KwdFor 
    "in" -> KwdIn 
    "while" -> KwdWhile 
    _ -> Identifier w

whereEnds :: Int -> (Char -> Bool) -> String -> Int
whereEnds x _ [] = x
whereEnds x f (s:ss)
  | s == '\\' = whereEnds (x+2) f (tail ss)
  | f s       = whereEnds (x+1) f ss
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
    ',' -> Just Comma
    '\\' -> Just Lambda
    '+' -> Just Plus
    '-' -> Just Minus
    '*' -> Just Times
    '/' -> Just Divide
    '=' -> Just Equal
    '%' -> Just Modulus
    _ -> Nothing
