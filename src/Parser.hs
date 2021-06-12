{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}

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
    | Bottom -- ?
    deriving (Eq, Show, Generic)

data Literal = Num Integer | Str Text
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass Print

data Program =
    Program
        [Declaration] -- constant and variable declaration
        [Expr]        -- Body

data ParseError
    = DefineUnmetArity
    | DefunUnmetArity
    | LambdaUnmetArity
    deriving (Show)

emptyProgram :: Program
emptyProgram = Program [] [] M.empty

(<+>) :: [a] -> a -> [a]
xs <+> x = concat [xs, x]
{-# inline (<+>) #-}

parse :: Program -> AbstSynTree -> Either ParseError Program
parse (Program decls exprs) (Atom atom) = case atom of
    Ident  str -> Right $ Program decls (exprs <+> Var str)
    Int    int -> Right $ Program decls (exprs <+> Lit (Num int))
    String str -> Right $ Program decls (exprs <+> Lit (Str str))
    Nil        -> Right $ Program decls exprs

parse prog (List (first:rest)) = case first of
    Atom (Ident "define")
        | [name, e] <- rest -> processDefine prog
        | otherwise -> Left DefineUnmetArity
    
    Atom (Ident "lambda")
        | [args, e] <- rest -> processLambda prog
        | otherwise  -> Left LambdaUnmetArity
    
    Atom (Ident "defun")
        | [name, args, e] <- rest -> processFunction prog
        | otherwise  -> Left DefunUnmetArity
    
    Atom atom | rest /= [] -> undefined

parse _prog (List []) = undefined -- unreachable

processAtom :: Atom -> Expr
processAtom (Ident name) = Var name
processAtom (Int int)    = Lit (Num int)
processAtom (String str) = Lit (Str str)
processAtom Nil          = Bottom

-- it works
processList :: [AbstSynTree] -> Expr
processList [] = undefined -- unreachable
processList (x:xs) = case x of
    Atom a -> processAtom a
    List xs' -> App (processList xs') (processList xs)

{-
processDefine :: Program -> Name -> AbstSynTree -> Program
processDefine (Program decls exprs) name (Atom a) = Program (decls <+> VarDecl name (processAtom a)) exprs
processDefine (Program decls exprs) name (List xs) = Program (decls ++ fmap processDefine xs) exprs
-}

-- processDefine = undefined
processLambda = undefined
processFunction = undefined
{-
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
-}