-- | Parsing

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}

module Parsing where

{-
This design is inspired by Sixty the repo, which is also inspired by
Andras Kovacs's flatparse module.
-}

import Parsing.Tokens
import qualified Parsing.Position as Position

newtype Parser a = Parser
  { runParser
    :: Consumed
    -> [Token] -- input
    -> ErrorReason -- previous errors at this position
    -> Position.LineColumn
    -> Position.Absolute -- offset, base position
    -> Result a
  }

data ErrorReason = ErrorReason { _reason :: Maybe String
                               , _expected :: String
                               } deriving Show

data Consumed = ConsumedNone
              | ConsumedSome
              deriving (Eq, Ord, Show)

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

mapResult :: (a -> b) -> Result a -> Result b
mapResult f (OK a consume input err) = OK (f a) consume input err
mapResult _ (Fail consume input err) = Fail consume input err

instance Functor Parser where
  fmap f (Parser p) = Parser \consume input err lineCol offset ->
    mapResult f (p consume input err lineCol offset)

--instance Applicative Parser where
--  pure a = Parser \consume input err _ _ -> OK a consume input err
