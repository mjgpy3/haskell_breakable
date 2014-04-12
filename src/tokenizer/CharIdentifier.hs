module CharIdentifier (isStringDelimeter,
                       isIgnored,
                       isIdentifier) where

isStringDelimeter :: Char -> Bool
isStringDelimeter = includes ['\'', '"']

isIgnored :: Char -> Bool
isIgnored = includes [' ', '\t']

isIdentifier :: Char -> Bool
isIdentifier = includes (['a'..'z'] ++ ['A'..'Z'] ++ ['_'])

-- Really just switches elem params
includes :: (Eq a) => [a] -> a -> Bool
includes xs x = elem x xs
