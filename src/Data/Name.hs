module Data.Name where

import qualified Data.ByteString as B
import Data.Hashable
import Hasher (fnv1a64)

newtype Name = Name { unName :: B.ByteString }
  deriving (Show, Eq) via B.ByteString

-- TODO: casting from Word64 to Int is ill-defined and not going to be bijective.
instance Hashable Name where
  hashWithSalt _salt (Name str) = fromIntegral (fnv1a64 str)
