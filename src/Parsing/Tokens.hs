-- | Contains all the tokens

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Parsing.Tokens where

import Prelude hiding (lex)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeIndex)
import Data.ByteString.Internal (w2c)
import Data.Word

import qualified Parsing.Position as Position
import qualified Parsing.Span as Span
 
data Token = Token !Position.LineColumn !Span.Absolute !Tok
  deriving Show

data Tok = Error
         | Let | In | Where -- re-bindings
         | Case | Of -- pattern matching case

         -- | reserved keywords
         | Equal
         | Dot
         | Colon
         | SemiColon
         | Underscore
         | LParen
         | RParen

         -- | Capturing groups
         | LowerIdent String
         | UpperIdent String
         | Operator   String
         | Number     Integer -- Haskell's Integer support Float & Double
         deriving Eq

instance Show Tok where
  show = \case
    Error -> "[unregconised token]"
    Let -> "let"
    In -> "in"
    Where -> "where"
    Case -> "case"
    Of -> "of"
    Equal -> "="
    Dot -> "."
    Colon -> ":"
    SemiColon -> ";"
    Underscore -> "_"
    LParen -> "("
    RParen -> ")"
    LowerIdent s -> show s
    UpperIdent s -> show s
    Operator s -> show s
    Number i -> show i

-- I feel like the simplest way of dealing with token is by
-- switch-case/pattern matching

data State = State { input :: ByteString
                   , end :: Position.Absolute
                   , position :: Position.Absolute
                   , lineColumn :: Position.LineColumn
                   }

lexBS :: ByteString -> [Token]
lexBS bs = lex State { input = bs
                     , end = Position.Absolute (BS.length bs)
                     , position = Position.Absolute 0
                     , lineColumn = Position.LineColumn 0 0
                     }

lex :: State -> [Token]
lex state@State {..}
  | position >= end = [] -- length guard
  | otherwise = let
      state1 = state { position = position1
                     , lineColumn = Position.addColumns lineColumn 1
                     }
      state2 = state { position = position2
                     , lineColumn = Position.addColumns lineColumn 2
                     }
      position1 = Position.add position (Position.Relative 1)
      position2 = Position.add position (Position.Relative 2)

      token1 = Token lineColumn (Span.Absolute position position1)
      token2 = Token lineColumn (Span.Absolute position position2)
    in
      case w2c (index input position) of
        '(' -> token1 LParen : lex state1
        ')' -> token1 RParen : lex state1
       
        _ -> token1 Error : lex state1

index :: ByteString -> Position.Absolute -> Word8
index bs (Position.Absolute idx) = unsafeIndex bs idx
