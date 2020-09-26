-- |
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
module Parsing.Tokenizer where

import Text.Regex.TDFA
import Parsing.Tokens

type Span = (Int, Int) -- (offset, length)

newtype Lexer a = Lexer { runLexer :: a -> Span }

-- | Will consume the string and return the span of matched result
-- | and the remaining string.
tokenRE :: Lexer String
tokenRE
  =   Lexer (=~ kwLetRE)
  <|> Lexer (=~ kwInRE)
  <|> Lexer (=~ kwWhereRE)
  <|> Lexer (=~ blockCommentRE)
  <|> Lexer (=~ lineCommentRE)
  <|> Lexer (=~ lowerIdentRE)
  <|> Lexer (=~ upperIdentRE)
  <|> Lexer (=~ floatingNumRE)
  <|> Lexer (=~ naturalNumRE)

-- | Choose between two lexers. The second parser is only tried
-- | if the first one fails without having consumed input.
infixr 6 <|>
(<|>) :: Lexer a -> Lexer a -> Lexer a
(<|>) (Lexer f) (Lexer g) = Lexer \a -> case f a of
  (-1, _) -> g a -- tdfa will return (-1, offset) if does not match
  x -> x
{-# INLINE (<|>) #-}
