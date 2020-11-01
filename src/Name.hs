{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Name where

import Data.Hashable (Hashable)
import Data.ByteString.Short (ShortByteString, toShort)

newtype Name = Name ShortByteString
  deriving (Eq, Ord, Show, Hashable)

newtype Module = Module ShortByteString
  deriving (Eq, Ord, Show)

toName = toShort