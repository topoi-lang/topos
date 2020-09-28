-- | Token spans information

module Parsing.Span where

import qualified Parsing.Position as Position

data Absolute = Absolute Position.Absolute Position.Absolute
  deriving (Eq, Ord, Show)

data LineColumn = LineColumn Position.LineColumn Position.LineColumn
  deriving (Eq, Ord, Show)
