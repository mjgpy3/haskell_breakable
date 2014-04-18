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
  | isJust double       = unjust double: tokenize (tail xs)
  | isJust single       = unjust single:tokenize xs
  | isStringDelimeter x = BfString str:tokenize (drop 1 postStr)
  | isNumeric x         = asBfNumber (x:num):tokenize postNum
  | isIdentifier x      = kwdOrIdent (x:ident):tokenize postIdent
  | otherwise           = error ("Unrecognized token: " ++ [x])
  where single = singleChar x
        double = doubleChar $ take 2 r
        (ident, postIdent) = split whereEnds isIdentifier xs
        (str, postStr) = split whereStringEnds (x /=) xs
        (num, postNum) = split whereNumberEndsOrDot isNumeric xs

split :: (Int -> (Char -> Bool) -> String -> Int) -> (Char -> Bool) -> String -> (String, String)
split checkEnd whereImTrue onString = splitAt (checkEnd 0 whereImTrue onString) onString

asBfNumber :: String -> BfToken
asBfNumber number
  | '.' `elem` number = BfFloat number
  | otherwise         = BfInteger number

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
  | s == '\\' = whereStringEnds (x+2) f (tail ss)
  | f s       = whereStringEnds (x+1) f ss
  | otherwise = x

whereEnds :: Int -> (Char -> Bool) -> String -> Int
whereEnds x _ [] = x
whereEnds x f (s:ss)
  | f s       = whereEnds (x+1) f ss
  | otherwise = x

whereNumberEndsOrDot :: Int -> (Char -> Bool) -> String -> Int
whereNumberEndsOrDot x f = whereNumberEnds x f False

whereNumberEnds :: Int -> (Char -> Bool) -> Bool -> String -> Int
whereNumberEnds x _ _ [] = x
whereNumberEnds x f dotFound (s:ss)
  | f s || unfoundAndCurrent = whereNumberEnds (x+1) f foundOrCurrent ss
  | otherwise                   = x
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
    '\\' -> Just Lambda
    '+' -> Just Plus
    '-' -> Just Minus
    '*' -> Just Times
    '/' -> Just Divide
    '=' -> Just Equal
    '%' -> Just Modulus
    _ -> Nothing
