module Syntax where

import FlatParse

-- TODO: subject to change
data Type = TNat | TArrow Pos Type Type
  deriving Show

{-
  A `Pos` is a byte offset into an implicit ByteString.
  A `Span` is a pair of `Pos`s.
-}

-- | This is the result of the parser. In parsing, it is more efficient to only store these.
-- since it is obvious which ByteString we are operating on.
data Expr
  = Var Span
  | App Expr Expr
  | Lam Span Type Expr
  | Let Pos Span (Maybe Type) Expr Expr
  deriving Show
