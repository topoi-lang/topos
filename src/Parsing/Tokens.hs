-- | Contains all the tokens

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.Tokens where

import Prelude hiding (lex)

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS

import Data.ByteString.Unsafe (unsafeIndex)
import Data.ByteString.Internal (w2c, c2w)
import Data.Word
import Data.Char

import Debug.Trace

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
         | LArrow
         | RArrow

         -- | Capturing groups
         | LowerIdent ShortByteString
         | UpperIdent ShortByteString
         | Operator   ShortByteString
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
    LArrow -> "->"
    RArrow -> "->"
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
                     , lineColumn = Position.LineColumn 1 0
                     }

-- | A bit switch case that will do all the lexing procedure.
-- | Noted that the parenthesis is a special case of the token

-- [TODO] maybe we can use pattern synonyms to abstract away the c2w
-- and pattern matching the char8
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

      char8 = index input position
    in
      case w2c char8 of
        ' ' -> lex state1 -- whitespace
        '\n' -> lex state1 { lineColumn = Position.addLine lineColumn }
        '(' -> token1 LParen : lex state1
        ')' -> token1 RParen : lex state1

        -- Sigle line comment, starts with '--'
        '-'
          | position1 < end
          , '-' <- w2c (index input position1) -> singleLineComment state2

        -- Block comment, starts with '{-' and ends with '-}'
        '{'
          | position1 < end
          , '-' <- w2c (index input position1) -> commentBlock state2 1

        -- Numbers starts with negative sign
        '-'
          | position1 < end
          , char8' <- (index input position1)
          , isDigit (w2c char8') ->
            number position lineColumn state2 True $
              fromIntegral (char8' - c2w '0')

        -- Number
        c | isDigit c -> number position lineColumn state1 False $
            fromIntegral (char8 - c2w '0')

        -- Identifier
        c | isUpperIdentifierStart c ->
            identifier position lineColumn state1 True

        c | isIdentifierStart c ->
            identifier position lineColumn state1 False

        -- Operators, mostly symbols
        c | isASCIIOperator c ->
            operator position lineColumn state1
           
        _ -> token1 Error : lex state1

index :: ByteString -> Position.Absolute -> Word8
index bs (Position.Absolute idx) = unsafeIndex bs idx

singleLineComment :: State -> [Token]
singleLineComment state@State {..}
  | position >= end = []
  | otherwise = let
      state1 = state { position = Position.add position (Position.Relative 1) }
    in case w2c (index input position) of
      '\n' -> lex state { lineColumn = Position.addLine lineColumn }
      _ -> singleLineComment state1

commentBlock :: State -> Int -> [Token]
commentBlock state 0 = lex state
commentBlock state@State {..} depth
  | position >= end = []
  | otherwise = let
      position1 = Position.add position (Position.Relative 1)
      position2 = Position.add position (Position.Relative 2)

      state1 = state { position = position1
                     , lineColumn = Position.addColumns lineColumn 1
                     }
      state2 = state { position = position2
                     , lineColumn = Position.addColumns lineColumn 2
                     }
    in case w2c (index input position) of
      '{'
        | position1 < end
        , '-' <- w2c (index input position1) ->
          commentBlock state2 (depth + 1)

      '-'
        | position1 < end
        , '}' <- w2c (index input position1) ->
          commentBlock state2 (depth - 1)

      '\n' -> commentBlock
                state1 { lineColumn = Position.addLine lineColumn }
                depth

      _ -> commentBlock state1 depth

number
  :: Position.Absolute
  -> Position.LineColumn
  -> State
  -> Bool
  -> Integer
  -> [Token]
number startPos startLineColumn state@State {..} shouldNegate acc
  | position >= end = [token]
  | otherwise = case w2c c8 of
      c | isDigit c -> do
            let acc' = acc * 10 + fromIntegral (c8 - c2w '0')
            number startPos startLineColumn state1 shouldNegate acc'
      _ -> token : lex state

  where
    c8 = index input position
    token = Token startLineColumn (Span.Absolute startPos position) $
      Number (if shouldNegate then negate acc else acc)

    state1 = state { position = Position.add position (Position.Relative 1)
                   , lineColumn = Position.addColumns lineColumn 1
                   }

isUpperIdentifierStart :: Char -> Bool
isUpperIdentifierStart c = isUpper c || isLetter c
{-# INLINEABLE isUpperIdentifierStart #-}

isIdentifierStart :: Char -> Bool
isIdentifierStart c = isAlpha c || c == '_'
{-# INLINEABLE isIdentifierStart #-}

isIdentifierRest :: Char -> Bool
isIdentifierRest c = isIdentifierStart c || isDigit c || c == '\''
{-# INLINEABLE isIdentifierRest #-}

identifier
  :: Position.Absolute
  -> Position.LineColumn
  -> State
  -> Bool -- is this starts with uppercase?
  -> [Token]
identifier startPos startLineColumn state@State {..} isUpperIdent
  | position >= end =
      [identifierToken input startPos startLineColumn position isUpperIdent]
  | otherwise = case w2c (index input position) of
      c | isIdentifierRest c ->
            identifier startPos startLineColumn state1 isUpperIdent
      _ ->
        identifierToken input startPos startLineColumn position isUpperIdent :
        lex state

  where
    position1 = Position.add position (Position.Relative 1)
    position2 = Position.add position (Position.Relative 1)

    state1 = state { position = position1
                   , lineColumn = Position.addColumns lineColumn 1
                   }
    state2 = state { position = position2
                   , lineColumn = Position.addColumns lineColumn 2
                   }

-- This can use a DFA state machine to do. But since our token set is so small.
identifierToken
  :: ByteString
  -> Position.Absolute
  -> Position.LineColumn
  -> Position.Absolute
  -> Bool -- is this starts with uppercase?
  -> Token
identifierToken input startPos startLineColumn position isUpperIdent =
  Token startLineColumn (Span.Absolute startPos position) $
    case w2c (index input startPos) of
      '_' | pos - base == 1 -> Underscore
      'l' | "let" <- str -> Let
      'i' | "in" <- str -> In
      'w' | "where" <- str -> Where
      'c' | "case" <- str -> Case
      'o' | "of" <- str -> Of
      _ -> let shortBS = SBS.toShort str in
        if isUpperIdent then UpperIdent shortBS else LowerIdent shortBS
  where
    (Position.Absolute base) = startPos
    (Position.Absolute pos) = position
    str = BS.drop base $ BS.take (base + pos) input

isASCIIOperator :: Char -> Bool
isASCIIOperator = \case
  '!' -> True
  '#' -> True
  '$' -> True
  '%' -> True
  '&' -> True
  '*' -> True
  '+' -> True
  '-' -> True
  '/' -> True
  '?' -> True
  '>' -> True
  '<' -> True
  '@' -> True
  '^' -> True
  '|' -> True
  '~' -> True
  _  -> False
{-# INLINABLE isASCIIOperator #-}

operator
  :: Position.Absolute
  -> Position.LineColumn
  -> State
  -> [Token]
operator startPos startLineColumn state@State {..}
  | position >= end =
      [operatorToken input startPos startLineColumn position]
  | otherwise = case w2c (index input position) of
      c | isASCIIOperator c ->
          operator startPos lineColumn state1
      _ ->
          operatorToken input startPos startLineColumn position :
          lex state
  where
    position1 = Position.add position (Position.Relative 1)
    state1 = state { position = position1
                   , lineColumn = Position.addColumns lineColumn 1
                   }

operatorToken
  :: ByteString
  -> Position.Absolute
  -> Position.LineColumn
  -> Position.Absolute
  -> Token
operatorToken input startPos startLineColumn position =
  Token startLineColumn (Span.Absolute startPos position) $
    case w2c (index input startPos) of
      '=' | len == 1 -> Equal
      ';' | len == 1 -> SemiColon
      ':' | len == 1 -> Colon
      '.' | len == 1 -> Dot
      '-' | "->" <- str -> RArrow
      '<' | "<-" <- str -> LArrow
      _ -> let shortBS = SBS.toShort str in Operator shortBS
  where
    (Position.Absolute base) = startPos
    (Position.Absolute pos) = position
    len = pos - base
    str = BS.drop base $ BS.take (base + pos) input
