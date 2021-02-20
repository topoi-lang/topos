module Hasher where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr (ForeignPtr(ForeignPtr)) 
import GHC.Exts (Addr#, Int(I#), plusAddr#, eqAddr#)
import Data.Word (Word8, Word64)
import Data.Bits
import Data.Primitive.Types (indexOffAddr#)

-- FNV-1a, some say this has better avalanche property compared to FNV-1.
-- | https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function#FNV-1a_hash
fnv1a64 :: B.ByteString -> Word64
fnv1a64 (B.PS (ForeignPtr ptr _) (I# offset) (I# len)) = go start end offsetBasis
  where
    offsetBasis = 14695981039346656037
    fnvPrime = 1099511628211
    start = plusAddr# ptr offset
    end = plusAddr# start len

    go :: Addr# -> Addr# -> Word64 -> Word64
    go startPtr endPtr hash = case eqAddr# startPtr endPtr of
      1# -> hash
      _  -> go (plusAddr# startPtr 1#) endPtr $ do
        let byte = indexOffAddr# startPtr 0# :: Word8
        (hash `xor` fromIntegral byte) * fnvPrime
{-# INLINE fnv1a64 #-}
