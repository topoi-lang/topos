-- | Position information allocated during parsing stage
module Parsing.Position where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import Data.ByteString.Internal (c2w)

newtype Absolute = Absolute Int
  deriving (Eq, Ord, Show)

newtype Relative = Relative Int
  deriving (Eq, Ord, Show)

data LineColumn = LineColumn !Int !Int
  deriving (Eq, Ord, Show)

relativeTo :: Absolute -> Absolute -> Relative
relativeTo (Absolute base) (Absolute pos) = Relative (pos - base)

add :: Absolute -> Relative -> Absolute
add (Absolute base) (Relative rel) = Absolute (base + rel)

addLine :: LineColumn -> LineColumn
addLine (LineColumn line _) = LineColumn (line + 1) 0

addColumns :: LineColumn -> Int -> LineColumn
addColumns (LineColumn line column) delta = LineColumn line (column + delta)

lineColumn :: Absolute -> ByteString -> (LineColumn, ByteString)
lineColumn (Absolute idx) bytestring =
  let prefix = BS8.take idx bytestring
      suffix = BS8.drop idx bytestring

      linePrefixLength = BS8.length $ BS.takeWhileEnd (/= c2w '\n') prefix
      lineSuffixLength = BS8.length $ BS.takeWhile (/= c2w '\n') suffix

      lineStart = idx - linePrefixLength
      lineLength = linePrefixLength + lineSuffixLength

      line = BS8.take lineLength $ BS8.drop lineStart bytestring
   in (LineColumn (BS.count (c2w '\n') prefix) linePrefixLength, line)
