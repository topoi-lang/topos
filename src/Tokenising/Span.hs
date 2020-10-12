-- | Token spans information

module Tokenising.Span where

import qualified Tokenising.Position as Position

data Absolute = Absolute Position.Absolute Position.Absolute
  deriving (Eq, Ord, Show)

data LineColumn = LineColumn Position.LineColumn Position.LineColumn
  deriving (Eq, Ord, Show)
