module Tokenizer
  (BfToken(..),
  tokenize) where

import CharIdentifier
import Data.List
import Data.Maybe

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
  Colon |
  Lambda |
  Arrow |
  Plus |
  Minus |
  Times |
  Divide |
  Modulus |
  Power |
  Assign |
  Equal | NotEqual |
  Lt | Gt |
  Leq | Geq
  deriving (Show, Eq)

tokenize :: String -> [BfToken]
tokenize [] = []
tokenize r@(x:xs)
  | isIgnored x         = tokenize xs
  | isJust double       = unjust double: tokenize (tail xs)
  | isJust single       = unjust single:tokenize xs
  | isStringDelimeter x = str:tokenize (drop 1 postStr)
  | isNumeric x         = num:tokenize postNum
  | isIdentifier x      = ident:tokenize postIdent
  | otherwise           = error ("Unrecognized token: " ++ [x])
  where single = singleChar x
        double = doubleChar $ take 2 r
        (ident, postIdent) = splitIdent [] r
        (str, postStr) = splitString [] x xs
        (num, postNum) = splitNumber [] False r

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

splitString :: String -> Char -> String -> (BfToken, String)
splitString x _ [] = (BfString x, [])
splitString x d (s:ss)
  | s == '\\' = splitString (x ++ [s] ++ [head ss]) d (tail ss)
  | s /= d    = splitString (x ++ [s]) d ss
  | otherwise = (BfString x, ss)

splitIdent :: String -> String -> (BfToken, String)
splitIdent b [] = (kwdOrIdent b, [])
splitIdent b w@(s:ss)
  | isIdentifier s = splitIdent (b ++ [s]) ss
  | otherwise      = (kwdOrIdent b, w)

splitNumber :: String -> Bool -> String -> (BfToken, String)
splitNumber b dotFound [] = (if dotFound then BfFloat b else BfInteger b, [])
splitNumber b dotFound w@(s:ss)
  | isNumeric s || unfoundAndCurrent = splitNumber (b ++ [s]) foundOrCurrent ss
  | dotFound                         = (BfFloat b, w)
  | otherwise                        = (BfInteger b, w)
  where currentIsDot = s == '.'
        foundOrCurrent = currentIsDot || dotFound
        unfoundAndCurrent = currentIsDot && not dotFound

doubleChar :: String -> Maybe BfToken
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
    ':' -> Just Colon
    '\\' -> Just Lambda
    '+' -> Just Plus
    '-' -> Just Minus
    '*' -> Just Times
    '/' -> Just Divide
    '=' -> Just Equal
    '%' -> Just Modulus
    '^' -> Just Power
    _ -> Nothing
