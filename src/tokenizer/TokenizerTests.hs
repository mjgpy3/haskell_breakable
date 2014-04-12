module TokenizerTests where

import Test.HUnit
import Tokenizer

test_empty_string = TestCase (assertEqual "for an empty string" [] (tokenize ""))
test_space = TestCase (assertEqual "for a space" [] (tokenize " "))
test_identifier = TestCase (assertEqual "for an identifier" [Identifier "x"] (tokenize "x"))
test_newline = TestCase (assertEqual "for a newline" [Newline] (tokenize "\n"))
test_open_paren = TestCase (assertEqual "for a (" [LeftParen] (tokenize "("))
test_close_paren = TestCase (assertEqual "for a )" [RightParen] (tokenize ")"))
test_dot = TestCase (assertEqual "for a ." [Dot] (tokenize "."))
test_plus = TestCase (assertEqual "for a +" [Plus] (tokenize "+"))
test_minus = TestCase (assertEqual "for a -" [Minus] (tokenize "-"))
test_times = TestCase (assertEqual "for a *" [Times] (tokenize "*"))
test_divide = TestCase (assertEqual "for a /" [Divide] (tokenize "/"))
test_modulus = TestCase (assertEqual "for a %" [Modulus] (tokenize "%"))
test_two_singles = TestCase (assertEqual "for two singles" [LeftParen, RightParen] (tokenize "()"))
test_single_then_space = TestCase (assertEqual "for a single then space" [Plus] (tokenize "+ "))
test_space_then_single = TestCase (assertEqual "for a space then single" [Minus] (tokenize " -"))

test_many_singles = TestCase (assertEqual
  "a bunch of singles"
  [Newline, LeftParen, RightParen, Dot, Plus, Minus, Times, Divide, Modulus]
  (tokenize "\n().+-*/%"))

test_underscore_is_identifier = TestCase (assertEqual "for _" [Identifier "_"] (tokenize "_"))
test_two_character_identifier = TestCase (assertEqual "for fo" [Identifier "fo"] (tokenize "fo"))
test_many_character_identifier = TestCase (assertEqual "for foo_bar" [Identifier "foo_bar"] (tokenize "foo_bar"))

test_empty_string_single_quotes = TestCase (assertEqual "for ''" [BfString []] (tokenize "''"))
test_empty_string_double_quotes = TestCase (assertEqual "for \"\"" [BfString []] (tokenize "\"\""))
test_nonempty_string = TestCase (assertEqual "for nonempty string" [BfString "a"] (tokenize "'a'"))
test_string_and_other_token = TestCase (assertEqual "for string and other token" [BfString "foo", Plus] (tokenize "'foo' +"))

tests = TestList 
  [test_empty_string,
  test_space,
  test_identifier,
  test_newline,
  test_open_paren,
  test_close_paren,
  test_dot,
  test_plus,
  test_minus,
  test_times,
  test_divide,
  test_modulus,
  test_two_singles,
  test_single_then_space,
  test_space_then_single,
  test_many_singles,
  test_underscore_is_identifier,
  test_two_character_identifier,
  test_many_character_identifier,
  test_empty_string_single_quotes,
  test_empty_string_double_quotes,
  test_nonempty_string,
  test_string_and_other_token]

main = runTestTT tests
