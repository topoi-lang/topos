module Name where

import Data.ByteString.Short ( ShortByteString, toShort )

newtype Name = Name ShortByteString
  deriving (Eq, Ord, Show)

newtype Module = Module ShortByteString
  deriving (Eq, Ord, Show)

toName = toShort