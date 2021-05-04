module Data.SpannedTerm where

import FlatParse.Stateful (Pos, Span)

{-|
This is the result of the parser. In parsing, it is more efficient to only
store these. Since it is obvious which ByteString we are operating on.

When the values are deep copied, it will be in the UnspannedTerm.hs.

TODO:
  - how are we reference back from the unspanned term to this spanned term?
    - for error reporting
-}

data Type
  = TNat
  | TBool
  | TCon Span
  | TArrow Pos Type Type
  deriving Show

data Tm
  = Var Span
  | App Tm Tm
  | Lam Pos Span (Maybe Type) Tm
  | LetIn Pos Span (Maybe Type) Tm Tm
  
  | Hole

deriving instance Show Tm
