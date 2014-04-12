module Tokenizer
  (BfToken(..),
  tokenize) where

import CharIdentifier
import Data.List

data BfToken = 
  Identifier String |
  BfString String |
  Newline |
  LeftParen |
  RightParen |
  Dot |
  Plus |
  Minus |
  Times |
  Divide |
  Modulus
  deriving (Show, Eq)

tokenize :: String -> [BfToken]
tokenize [] = []
tokenize (x:xs)
  | isIgnored x       = tokenize xs
  | single /= Nothing = (unjust single):tokenize xs
  | isStringDelimeter x = (BfString (fst stringSplit)):tokenize (drop 1 (snd stringSplit))
  | otherwise         = (Identifier (x:(fst identSplit))):tokenize (snd identSplit)
  where single = singleChar x
        identSplit = splitAt (whereEnds 0 isIdentifier xs) xs
        stringSplit = splitAt (whereEnds 0 (not.isStringDelimeter) xs) xs

unjust :: Maybe a -> a
unjust Nothing = error "unjust should never be called on Nothing"
unjust (Just a) = a

whereEnds :: Int -> (Char -> Bool) -> String -> Int
whereEnds x _ [] = x
whereEnds x f (s:ss)
  | f s       = whereEnds (x+1) f ss
  | otherwise = x

singleChar :: Char -> Maybe BfToken
singleChar x =
  case x of
    '\n' -> Just Newline
    '(' -> Just LeftParen
    ')' -> Just RightParen
    '.' -> Just Dot
    '+' -> Just Plus
    '-' -> Just Minus
    '*' -> Just Times
    '/' -> Just Divide
    '%' -> Just Modulus
    _ -> Nothing
