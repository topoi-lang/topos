module Data.FNV1a where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.Exts
  ( Addr#,
    Int (I#),
    Int#,
    eqAddr#,
    indexInt8OffAddr#,
    plusAddr#,
    xorI#,
    (*#),
  )
import GHC.ForeignPtr (ForeignPtr (ForeignPtr))

-- | FNV-1a, it has better avalanche property compared to FNV-1. Suitable for
-- small inputs like hostnames, IP addresses, and filenames.
-- https://create.stephan-brumme.com/fnv-hash/
fnv1a64 :: B.ByteString -> Int -> Int
fnv1a64 (B.PS (ForeignPtr ptr _) (I# offset) (I# len)) (I# salt) =
  I# (go start (plusAddr# start len) salt)
  where
    fnvPrime = 1099511628211#
    start = plusAddr# ptr offset

    go :: Addr# -> Addr# -> Int# -> Int#
    go startPtr endPtr hash = case eqAddr# startPtr endPtr of
      1# -> hash
      _ -> go (plusAddr# startPtr 1#) endPtr
          (hash `xorI#` indexInt8OffAddr# startPtr 0#) *# fnvPrime

{-# INLINE fnv1a64 #-}
