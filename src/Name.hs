-- | This needs to be documented seriously

module Name where

import Data.ByteString.Short (ShortByteString)

newtype Pre = Pre ShortByteString
  deriving (Eq, Ord, Show)

newtype Name = Name ShortByteString
  deriving (Eq, Ord, Show)

newtype Module = Module ShortByteString
  deriving (Eq, Ord, Show)
