module Term where

import Name

-- The STLC is based on the (untyped) lambda calculus.
-- And the syntax of the STLC consists of two things: terms and types
data Term
  = Lam Name Type Term -- Lambda introduces type
  | Var Name
  | App Term Term
  | Lit Lit
  | BinOps PrimOp Term Term
  deriving (Show)

data PrimOp = Add | Sub deriving (Show)

data Lit
  = LInt Int
  | LBool Bool
  | LString String
  deriving (Show)

data Type
  = TInt
  | TBool
  | TClosure Type Type -- A closure
  deriving (Show, Eq)