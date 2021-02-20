module Test.Int2Word64IsBijectiveSpec (spec) where

import Test.Syd
import Data.Word
import Data.Bits.Extras
import Test.QuickCheck

spec :: Spec
spec =
  describe "Numbers" .
    specify "Mapping from Word64 to Int is bijection" .
      property $ \n -> w64 (fromIntegral n) `shouldBe` (n :: Word64)