module CharIdentifier (isStringDelimeter,
                       isIgnored,
                       isIdentifier,
                       isNumeric) where

isStringDelimeter :: Char -> Bool
isStringDelimeter = includes "'\""

isIgnored :: Char -> Bool
isIgnored = includes " \t"

isIdentifier :: Char -> Bool
isIdentifier = includes (['a'..'z'] ++ ['A'..'Z'] ++ "_")

isNumeric :: Char -> Bool
isNumeric = includes numbers

numbers = ['0'..'9']

-- Really just switches elem params
includes :: (Eq a) => [a] -> a -> Bool
includes xs x = x `elem` xs
