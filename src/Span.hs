-- | Token spans information

module Span where

import qualified Position

data Absolute = Absolute Position.Absolute Position.Absolute
  deriving (Eq, Ord, Show)

data LineColumn = LineColumn Position.LineColumn Position.LineColumn
  deriving (Eq, Ord, Show)

data Relative = Relative Position.Relative Position.Relative
  deriving (Eq, Ord, Show)

relativeTo :: Position.Absolute -> Span.Absolute -> Span.Relative
relativeTo base (Span.Absolute start end) =
  Span.Relative (Position.relativeTo base start) (Position.relativeTo base end)