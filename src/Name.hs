-- | This needs to be documented seriously

module Name where

import Data.ByteString.Short (ShortByteString)

newtype Surface = Surface ShortByteString
  deriving (Eq, Ord, Show)

newtype Name = Name ShortByteString
  deriving (Eq, Ord, Show)

newtype Module = Module ShortByteString
  deriving (Eq, Ord, Show)
