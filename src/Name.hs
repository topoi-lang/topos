{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Name where

import Data.Hashable (Hashable)
import Data.ByteString.Short (ShortByteString, toShort)
import qualified Data.ByteString.UTF8 as BSU

newtype Name = Name ShortByteString
  deriving (Eq, Ord, Show, Hashable)

newtype Module = Module ShortByteString
  deriving (Eq, Ord, Show)

-- toName = toShort

strToName :: String -> Name
strToName = Name . toShort . BSU.fromString
