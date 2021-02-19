module Data.Name where

import qualified Data.ByteString as B
import Data.Hashable
import Hasher (fnv1a64)

newtype RawName = RawName { unRawName :: B.ByteString }
  deriving (Show, Eq) via B.ByteString 

instance Hashable RawName where
  hashWithSalt _salt (RawName str) = fromIntegral (fnv1a64 str)
