module CharIdentifierTests where

import Test.HUnit
import CharIdentifier

isTrueForAnyOf _ [] = False
isTrueForAnyOf f (x:xs)
  | f x       = True
  | otherwise =  isTrueForAnyOf f xs

isTrueForAllOf _ [] = True
isTrueForAllOf f (x:xs)
  | not $ f x = False
  | otherwise = isTrueForAllOf f xs

test_single_quote_is_string_delimiter = TestCase (assertEqual "for '" True (isStringDelimeter '\''))
test_double_quote_is_string_delimiter = TestCase (assertEqual "for \"" True (isStringDelimeter '"'))

letters = ['a'..'z'] ++ ['A'..'Z']
nonLetters = ['%', '&', '\n']
numbers = ['0'..'9']
otherCharacters = letters ++ nonLetters ++ numbers

test_non_quote_characters_not_string_delimiters = TestCase (assertEqual "for others" False (isTrueForAnyOf isStringDelimeter otherCharacters))

test_space_is_ignored = TestCase (assertEqual "for space" True (isIgnored ' '))
test_tab_is_ignored = TestCase (assertEqual "for tab" True (isIgnored '\t'))
test_other_characters_are_not_ignored = TestCase (assertEqual "for others" False (isTrueForAnyOf isIgnored otherCharacters))

test_any_letter_is_an_identifier = TestCase (assertEqual "for characters" True (isTrueForAllOf isIdentifier letters))
test_underscore_is_an_identifier = TestCase (assertEqual "for _" True (isIdentifier '_'))
test_various_other_characters_are_not_identifiers = 
  TestCase (assertEqual "for various others" False (isTrueForAnyOf isIdentifier (nonLetters ++ ['2', '#', '@'])))

test_each_number_is_numeric = TestCase (assertEqual "for numbers" True (isTrueForAllOf isNumeric numbers))

tests = TestList 
  [test_single_quote_is_string_delimiter,
  test_double_quote_is_string_delimiter,
  test_non_quote_characters_not_string_delimiters,
  test_space_is_ignored,
  test_tab_is_ignored,
  test_other_characters_are_not_ignored,
  test_any_letter_is_an_identifier,
  test_underscore_is_an_identifier,
  test_various_other_characters_are_not_identifiers,
  test_each_number_is_numeric]

main = runTestTT tests
