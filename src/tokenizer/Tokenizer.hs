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
  Newline |
  LeftCurl | RightCurl |
  LeftParen | RightParen |
  LeftSquare | RightSquare |
  Dot |
  Comma |
  Plus |
  Minus |
  Times |
  Divide |
  Modulus
  deriving (Show, Eq)

tokenize :: String -> [BfToken]
tokenize [] = []
tokenize (x:xs)
  | isIgnored x         = tokenize xs
  | single /= Nothing   = (unjust single):tokenize xs
  | isStringDelimeter x = (BfString (fst stringSplit)):tokenize (drop 1 (snd stringSplit))
  | isNumeric x         = (BfInteger (x:(fst numberSplit))):tokenize (snd numberSplit)
  | isIdentifier x      = (kwdOrIdent (x:(fst identSplit))):tokenize (snd identSplit)
  | otherwise           = error ("Unrecognized token: " ++ [x])
  where single = singleChar x
        identSplit = splitAt (whereEnds 0 isIdentifier xs) xs
        stringSplit = splitAt (whereEnds 0 ((/=) x) xs) xs
        numberSplit = splitAt (whereEnds 0 isNumeric xs) xs

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
    _ -> Identifier w

whereEnds :: Int -> (Char -> Bool) -> String -> Int
whereEnds x _ [] = x
whereEnds x f (s:ss)
  | s == '\\' = whereEnds (x+2) f (tail ss)
  | f s       = whereEnds (x+1) f ss
  | otherwise = x

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
    '.' -> Just Dot
    ',' -> Just Comma
    '+' -> Just Plus
    '-' -> Just Minus
    '*' -> Just Times
    '/' -> Just Divide
    '%' -> Just Modulus
    _ -> Nothing
