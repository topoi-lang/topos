-- | Parsing utilities for Parsing.hs

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-} -- For the 'input'

module Parsing.Parser where

{-
This design is inspired by Sixty the repo, which is also inspired by
Andras Kovacs's flatparse module.
-}

import Control.Applicative
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.ByteString (ByteString)

import Parsing.Tokens
import qualified Parsing.Position as Position
import qualified Parsing.Span as Span
import qualified Error.Parsing as Error

parseTokens :: Parser a -> [Token] -> Either Error.Parsing a
parseTokens p tokens =
  case runParser
         p
         ConsumedNone
         tokens
         mempty
         (Position.LineColumn 0 0)
         (Position.Absolute 0) of
    OK a _ _ _ -> Right a
    Fail _ tokens' err ->
      Left Error.Parsing { Error.reason = _reason err
                         , Error.expected = HashSet.toList $ _expected err
                         , Error.position = case tokens' of
                             [] -> Left Error.EOF
                             Token _ (Span.Absolute pos _) _:_ -> Right pos
                         }
newtype Parser a = Parser
  { runParser
    :: Consumed
    -> [Token] -- input
    -> ErrorReason -- previous errors at this position
    -> Position.LineColumn
    -> Position.Absolute -- offset, base position
    -> Result a
  }

data ErrorReason = ErrorReason { _reason :: Maybe ByteString
                               , _expected :: HashSet ByteString
                               } deriving Show

failed :: ByteString -> ErrorReason
failed reason = ErrorReason (Just reason) mempty

expected :: ByteString -> ErrorReason
expected str = ErrorReason Nothing (HashSet.singleton str)

instance Semigroup ErrorReason where
  ErrorReason r1 e1 <> ErrorReason r2 e2 = ErrorReason (r1 <|> r2) (e1 <> e2)

instance Monoid ErrorReason where
  mempty = ErrorReason empty mempty

data Consumed = ConsumedNone | ConsumedSome deriving (Eq, Ord, Show)

type Option a = (# a | (##) #)

pattern Some :: a -> Option a
pattern Some a = (# a | #)

pattern None :: Option a
pattern None = (# | (##) #)

{-# complete Some, None #-}

type Result a = (# Option a, Consumed, [Token], ErrorReason #)

pattern OK :: a -> Consumed -> [Token] -> ErrorReason -> Result a
pattern OK a consume input err = (# Some a, consume, input, err #)

pattern Fail :: Consumed -> [Token] -> ErrorReason -> Result a
pattern Fail consume input err = (# None, consume, input, err #)

{-# complete OK, Fail #-}

-- I need this to map the function @f@ inside the @a@, without touching the
-- original input
mapResult :: (a -> b) -> Result a -> Result b
mapResult f (OK a consume input err) = OK (f a) consume input err
mapResult _ (Fail consume input err) = Fail consume input err

instance Functor Parser where
  fmap f (Parser p) = Parser \consume input err lineCol base ->
    mapResult f (p consume input err lineCol base)

instance Applicative Parser where
  pure a = Parser \consume input err _ _ -> OK a consume input err

  (Parser p) <*> (Parser q) = Parser \consume input err lineCol base ->
    case p consume input err lineCol base of
      OK f consume' input' err' ->
        mapResult f (q consume' input' err' lineCol base)

      Fail consume' input' err' -> Fail consume' input' err'

  (Parser p) *> (Parser q) = Parser \consume input err lineCol base ->
    case p consume input err lineCol base of
      OK _ consume' input' err' -> q consume' input' err' lineCol base
      Fail consume' input' err' -> Fail consume' input' err'

  (Parser p) <* (Parser q) = Parser \consume input err lineCol base ->
    case p consume input err lineCol base of
      OK a consume' input' err' ->
        mapResult (const a) (q consume' input' err' lineCol base)

      x -> x

instance Alternative Parser where
  empty = Parser \consume input err _ _ -> Fail consume input err

  (Parser p) <|> (Parser q) = Parser \consume input err lineCol base ->
    case p ConsumedNone input err lineCol base of
      OK a consume' input' err' -> OK a (max consume consume') input' err'
      Fail ConsumedNone _ err'  -> q consume input err' lineCol base
      f@(Fail ConsumedSome _ _) -> f

instance Monad Parser where
  Parser f >>= p = Parser \consume input err lineCol base ->
    case f consume input err lineCol base of
      OK a consume' input' err' ->
        runParser (p a) consume' input' err' lineCol base

      Fail consume' input' err' ->
        Fail consume' input' err'

  (>>) = (*>)

error :: ErrorReason -> Parser a
error err = Parser \consume input err' _ _ -> Fail consume input (err' <> err)

-- try will not consume the input when failed to match
try :: Parser a -> Parser a
try (Parser p) = Parser \consume input err lineCol base ->
  case p consume input err lineCol base of
    ok@OK {} -> ok
    Fail {} -> Fail consume input err

eof :: Parser ()
eof = Parser \consume input err _ _ -> case input of
  [] -> OK () consume input err
  _:_ -> Fail consume input (err <> expected "EOF")

-- expect
(<?>) :: Parser a -> ByteString -> Parser a
Parser p <?> expect = Parser \consume input err lineCol base ->
  case p consume input err lineCol base of
    (# a, consume', input', err' #) ->
      (# a
       , consume'
       , input'
       , err' { _expected = HashSet.insert expect (_expected err') }
      #)

notFollowedBy :: Parser ByteString -> Parser ()
notFollowedBy (Parser p) = Parser \consume input err lineCol base ->
  case p consume input err lineCol base of
    OK a _ _ _ ->
      Fail consume input $ err <> failed ("Unexpected '" <> a <> "'")
    Fail {} ->
      OK () consume input err

withRecovery
  :: (ErrorReason -> Position.Absolute -> [Token] -> Parser a)
  -> Parser a
  -> Parser a
withRecovery recoverStrategy (Parser p) =
  Parser \consume input err lineCol base ->
    case p consume input err lineCol base of
      ok@OK {} -> ok

      fail@(Fail _ input' err') ->
        case runParser
               (recoverStrategy err' base input')
               consume
               input
               err
               lineCol
               base of
          ok@OK {} -> ok
          Fail {} -> fail