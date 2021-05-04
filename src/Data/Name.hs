module Data.Name where

import qualified Data.ByteString as B
import Data.Hashable
import Data.FNV1a (fnv1a64)

newtype Name = Name { unName :: B.ByteString }
  deriving (Show, Eq) via B.ByteString

instance Hashable Name where
  hashWithSalt salt (Name str) = fnv1a64 str salt

{- NOTE
    * The serialised name should have int size (how to solve the hash collisions? or do I need it?)
-}
