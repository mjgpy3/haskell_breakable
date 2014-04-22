module TokenizerTests where

import Test.HUnit
import Tokenizer

test_empty_string = TestCase (assertEqual "for an empty string" [] (tokenize ""))
test_space = TestCase (assertEqual "for a space" [] (tokenize " "))
test_tab = TestCase (assertEqual "for a tab" [] (tokenize "\t"))
test_comma = TestCase (assertEqual "for a comma" [Comma] (tokenize ","))
test_colon = TestCase (assertEqual "for a colon" [Colon] (tokenize ":"))
test_identifier = TestCase (assertEqual "for an identifier" [Identifier "x"] (tokenize "x"))
test_newline = TestCase (assertEqual "for a newline" [Newline] (tokenize "\n"))
test_open_paren = TestCase (assertEqual "for a (" [LeftParen] (tokenize "("))
test_close_paren = TestCase (assertEqual "for a )" [RightParen] (tokenize ")"))
test_open_square = TestCase (assertEqual "for [" [LeftSquare] (tokenize "["))
test_close_square = TestCase (assertEqual "for ]" [RightSquare] (tokenize "]"))
test_open_curl = TestCase (assertEqual "for {" [LeftCurl] (tokenize "{"))
test_close_curl = TestCase (assertEqual "for }" [RightCurl] (tokenize "}"))
test_dot = TestCase (assertEqual "for a ." [Dot] (tokenize "."))
test_pipe = TestCase (assertEqual "for a |" [Pipe] (tokenize "|"))
test_equal = TestCase (assertEqual "for =" [Equal] (tokenize "="))
test_gt = TestCase (assertEqual "for >" [Gt] (tokenize ">"))
test_lt = TestCase (assertEqual "for <" [Lt] (tokenize "<"))
test_lambda = TestCase (assertEqual "for \\" [Lambda] (tokenize "\\"))
test_plus = TestCase (assertEqual "for a +" [Plus] (tokenize "+"))
test_minus = TestCase (assertEqual "for a -" [Minus] (tokenize "-"))
test_times = TestCase (assertEqual "for a *" [Times] (tokenize "*"))
test_divide = TestCase (assertEqual "for a /" [Divide] (tokenize "/"))
test_modulus = TestCase (assertEqual "for a %" [Modulus] (tokenize "%"))
test_power = TestCase (assertEqual "for a ^" [Power] (tokenize "^"))
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
test_single_quotes_can_contain_double = TestCase (assertEqual "for single containing double" [BfString "\""] (tokenize "'\"'"))
test_double_quotes_can_contain_single = TestCase (assertEqual "for double containing single" [BfString "'"] (tokenize "\"'\""))
test_singles_with_escaped_single = TestCase (assertEqual "for '\\''" [BfString "\\'"] (tokenize "'\\''"))

test_lone_number_is_integer = TestCase (assertEqual "for 1 num" [BfInteger "4"] (tokenize "4"))
test_multiple_nums_in_line_make_integer = TestCase (assertEqual "for muliple nums" [BfInteger "42"] (tokenize "42"))
test_int_with_other_tokens = TestCase (assertEqual "for int with others" [BfInteger "42" , Plus] (tokenize "42 + "))

test_floats_have_periods = TestCase (assertEqual "for float" [BfFloat "4.2"] (tokenize "4.2"))
test_float_with_longer_beg = TestCase (assertEqual "for xxxx.y" [BfFloat "42.2"] (tokenize "42.2"))
test_float_and_others = TestCase (assertEqual "for float + float" [BfFloat "4.2", Plus, BfFloat "3.14"] (tokenize "4.2 + 3.14"))
test_float_and_dot = TestCase (assertEqual "for 4.2." [BfFloat "4.2", Dot] (tokenize "4.2."))

test_double_char_and_others = TestCase (assertEqual "for /= 4" [NotEqual, BfInteger "4"] (tokenize "/= 4"))

test_class = TestCase (assertEqual "kwd class" [KwdClass] (tokenize "class"))
test_meth = TestCase (assertEqual "kwd meth" [KwdMeth] (tokenize "meth"))
test_module = TestCase (assertEqual "kwd module" [KwdModule] (tokenize "module"))
test_not = TestCase (assertEqual "kwd not" [KwdNot] (tokenize "not"))
test_and = TestCase (assertEqual "kwd and" [KwdAnd] (tokenize "and"))
test_or = TestCase (assertEqual "kwd or" [KwdOr] (tokenize "or"))
test_if = TestCase (assertEqual "kwd if" [KwdIf] (tokenize "if"))
test_else = TestCase (assertEqual "kwd else" [KwdElse] (tokenize "else"))
test_for = TestCase (assertEqual "kwd for" [KwdFor] (tokenize "for"))
test_in = TestCase (assertEqual "kwd in" [KwdIn] (tokenize "in"))
test_while = TestCase (assertEqual "kwd while" [KwdWhile] (tokenize "while"))

test_assign = TestCase (assertEqual "for assign" [Assign] (tokenize ":="))
test_neq = TestCase (assertEqual "for neq" [NotEqual] (tokenize "/="))
test_leq = TestCase (assertEqual "for leq" [Leq] (tokenize "<="))
test_geq = TestCase (assertEqual "for geq" [Geq] (tokenize ">="))
test_arrow = TestCase (assertEqual "for ->" [Arrow] (tokenize "->"))

tests = TestList 
  [test_empty_string,
  test_space,
  test_tab,
  test_identifier,
  test_newline,
  test_open_paren,
  test_close_paren,
  test_dot,
  test_pipe,
  test_equal,
  test_gt,
  test_lt,
  test_lambda,
  test_arrow,
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
  test_string_and_other_token,
  test_lone_number_is_integer,
  test_multiple_nums_in_line_make_integer,
  test_int_with_other_tokens,
  test_single_quotes_can_contain_double,
  test_double_quotes_can_contain_single,
  test_singles_with_escaped_single,
  test_class,
  test_meth,
  test_module,
  test_power,
  test_not,
  test_open_square,
  test_comma,
  test_colon,
  test_open_curl,
  test_close_curl,
  test_and,
  test_or,
  test_if,
  test_else,
  test_for,
  test_in,
  test_while,
  test_assign,
  test_neq,
  test_leq,
  test_geq,
  test_floats_have_periods,
  test_float_with_longer_beg,
  test_float_and_others,
  test_double_char_and_others,
  test_float_and_dot]

main = runTestTT tests
