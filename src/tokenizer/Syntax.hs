module Syntax where

import Tokenizer

data AstNode = AstRoot [BfToken] deriving (Eq, Show)

makeAst :: [BfToken] -> AstNode
makeAst [] = AstRoot []
makeAst (x:xs)
  | length xs == 0 = AstRoot [Identifier nodeValue]
  | length xs == 1 = AstRoot [Identifier nodeValue, Identifier (getValue $ head xs)]
  where nodeValue = getValue x

getValue :: BfToken -> String
getValue (Identifier a) = a
