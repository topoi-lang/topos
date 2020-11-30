{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Term where

import Name

import Data.Hashable (Hashable)
import Data.ByteString (ByteString)

-- The STLC is based on the (untyped) lambda calculus.
-- And the syntax of the STLC consists of two things: terms and types
data Term
  = Lam Name Type Term -- Lambda introduces type
  | Var Name
  | App Term Term
  | Lit Lit
  | BinOps PrimOp Term Term
  deriving (Show)

data Lit
  = LInt Int
  | LBool Bool
  | LString ByteString
  deriving (Show)

newtype TVar = TV Name
  deriving (Show, Eq, Ord, Hashable)

data Type
  = TVar TVar
  | TConstructor String
  | TClosure Type Type -- A closure
  deriving (Show, Eq)

tInt, tBool, tString :: Type
tInt = TConstructor "Int"
tBool = TConstructor "Bool"
tString = TConstructor "String"

data PrimOp = Add | Sub | Mul | Div
  deriving (Show)
