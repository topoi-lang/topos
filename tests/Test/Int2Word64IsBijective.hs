module Test.Int2Word64IsBijective (spec) where

import Test.Syd
import Data.Word
import Data.Bits.Extras
import Test.QuickCheck

spec :: Spec
spec =
  describe "Word64" .
    specify "Mapping from Word64 to Int is bijection" .
      property $ \n -> w64 (fromIntegral n) `shouldBe` (n :: Word64)