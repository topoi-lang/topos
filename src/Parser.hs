{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}

module Parser where

import GHC.Generics (Generic)
import Z.Data.Text (Text, Print)
import Z.Data.Vector.FlatMap (FlatMap)

import qualified Z.Data.Vector.FlatMap as M
import qualified Z.Data.Builder as B

import Tokeniser
    ( Atom (Ident, Int, String, Nil)
    , AbstSynTree (Atom, List)
    )

type Name = Text
type Scope = FlatMap Name Expr

data Declaration
    = VarDecl Name Expr
    | FunDecl Name   -- ^ function name
              [Name] -- ^ arguments
              Expr   -- ^ function body
    | EnumDecl Name [Expr] 
    deriving (Eq, Show, Generic)

data Expr
    = Var Name
    | App Expr Expr
    | Lam Name Expr
    | Lit Literal
    deriving (Eq, Show, Generic)

data Literal = Int Integer | String Text
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass Print

data Program =
    Program
        [Declaration] -- constant and variable declaration
        [Expr]        -- Body
        Scope         -- Context for substitution

data ParseError
    = DefineUnmetArity
    | DefunUnmetArity
    | LambdaUnmetArity
    deriving (Show)

parse :: [AbstSynTree] -> Either ParseError Program
parse [] = Right (Program [] [] M.empty)
parse (first:rest) = case first of

    Atom (Ident "define")
        | [name, e] <- rest -> undefined
        | otherwise -> Left DefineUnmetArity

    Atom (Ident "lambda")
        | [args, e ] <- rest -> undefined
        | otherwise -> Left LambdaUnmetArity

    Atom (Ident "defun")
        | [name, args, e] <- rest -> undefined
        | otherwise -> Left DefunUnmetArity

    Atom (Ident "do") -> undefined -- use rest

    _ -> undefined
