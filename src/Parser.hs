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
    | FunDecl Name [Name] Expr
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

data Statement = Statement

data Program =
    Program
        [Declaration] -- constant and variable declaration
        [Expr]        -- Body
        Scope         -- Context for substitution
        [Statement]   -- Control Flow

data ParseError
    = UnableToApply
    deriving (Show)

parse :: [AbstSynTree] -> Either ParseError Program
parse [] = Right (Program [] [] M.empty [])
parse (first:rest) = case first of

    Atom (Ident "let")
        | [name, e] <- rest -> do
            Right (Program [] [] M.empty [])

    _ -> undefined
