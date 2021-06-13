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

{-

(define x expression)
(defun name arg expression)

(enum name (identifier))
    eg:
    (enum BOOL (true false))

(x) is equivalent to x, vice versa
therefore...
    (define x (23)) is equivalant to (define x 23)
    (defun name x (+ x 1)) is equivalent to (defun name (x) (+ x 1))
    (define plusOne (lambda x x + 1)) is equivalent to (defun plusOne x (+ x 1))

-}
data Declaration
    = VarDecl Name Expr
    | FunDecl Name   -- ^ function name
              [Name] -- ^ arguments
              Expr   -- ^ function body
    | EnumDecl Name [Expr] 
    deriving (Eq, Show, Generic)

{-

literal will be parsed as Lit.
    (1) will be pased as (Lit (Num 1))
    ("abc") will be parsed as (Lit (Str "abc"))
    () will be just Bottom


identifier will be parsed as Var.
    (someFunc) will be parsed as (Var "someFunc")


(fn arg) will be parsed as (App fn arg)
(fn a1 a2) will be parsed as (App (App fn a1) a2)

(lambda name expression) will be parsed as (Lam name expression)



for example:
    (define plusOne (lambda "x" (+ x 1)))

(+ x 1) will be parsed as
    (App (App "+"" (Var "x")) (Lit (Num 1)))

(lambda "x" expression) will be parsed as
    (Lam "x" expression)

together will be
    (Lam "x" (App (App "+"" (Var "x")) (Lit (Num 1))))

and (define x (lambda arg expr)) is equivalent to (defun x arg expr), therefore it becomes
    (FunDecl "plusOne" "x" (Lam "x" (App (App "+"" (Var "x")) (Lit (Num 1)))))

-}
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

{-

name = letter | symbol
string_literal = '"', { letter | digit }, '"'
expression = "(", { atom }, ")"

atom = name | symbol | declarations | application

declarations = defineDecl | defunDecl | enumDecl
defineDecl = "define", { name }, { expression }
defunDecl = "defun", { name }, "(", { name+ }, ")", { expression }
enumDecl = "enum", { name }, "(", {name+}, ")"

application = { name+ }

-}
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
xs <+> x = reverse (x:xs)
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
    Atom (Ident "define") -> undefined
    Atom (Ident "defun")  -> undefined
    Atom (Ident "enum")   -> undefined
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