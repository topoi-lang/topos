module Syntax where

import FlatParse.Stateful (Pos, Span)

-- TODO: subject to change
data Type = TNat | TBool | TArrow Pos Type Type
  deriving (Show, Eq)

{-
  A `Pos` is a byte offset into an implicit ByteString.
  A `Span` is a pair of `Pos`s.
-}

-- | This is the result of the parser. In parsing, it is more efficient to only store these.
-- since it is obvious which ByteString we are operating on.
data Expr
  = Var Span
  | App Expr Expr
  | Lam Pos Span (Maybe Type) Expr
  | Let Pos Span (Maybe Type) Expr Expr -- let (x : a) = t in u OR let x = t in u

  | T    Span
  | F    Span
  | Zero Span
  | Succ Expr
  | Pred Expr
  | IsZero Expr
  deriving Show
