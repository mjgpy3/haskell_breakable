module SyntaxTests where

import Test.HUnit
import Syntax
import Tokenizer

empty_is_root =
  TestCase (assertEqual "for empty" (AstRoot []) (makeAst []))
ident_only_is_root_ident =
  TestCase (assertEqual "for ident" (AstRoot [Identifier "foo"]) (makeAst [Identifier "foo"]))
other_ident_only_is_root_ident =
  TestCase (assertEqual "for other ident" (AstRoot [Identifier "blah"]) (makeAst [Identifier "blah"]))
two_identifiers =
  TestCase (assertEqual "for two idents" (AstRoot [Identifier "walt", Identifier "jessie"]) (makeAst [Identifier "walt", Identifier "jessie"]))

tests = TestList
  [empty_is_root,
  ident_only_is_root_ident,
  other_ident_only_is_root_ident,
  two_identifiers]

main = runTestTT tests
