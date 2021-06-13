{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}

module Parser where

import GHC.Generics (Generic, Par1)
import Z.Data.Text (Text, Print)

import qualified Z.Data.Builder as B

import Tokeniser
    ( Atom (Ident, Int, String, Nil)
    , AbstSynTree (Atom, List)
    )

type Name = Text

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
data Program = Program
    { declarations :: [Declaration] -- constant and variable declaration
    , exprs        :: [Expr]        -- Body
    }
    deriving (Show)

data ParseError
    = DefineUnmetArity
    | DefunUnmetArity
    | LambdaUnmetArity
    | NotValidName
    | NotValidArg
    deriving (Show)

processAtom :: Atom -> Expr
processAtom (Ident name) = Var name
processAtom (Int int)    = Lit (Num int)
processAtom (String str) = Lit (Str str)
processAtom Nil          = Bottom

isDeclarationKeyword :: Atom -> Bool
isDeclarationKeyword = \case
    (Ident "define") -> True
    (Ident "lambda") -> True
    (Ident "defun")  -> True
    (Ident "enum")   -> True
    _                -> False

-- it works
processExpr :: AbstSynTree -> Expr
processExpr (Atom a) = if isDeclarationKeyword a then Bottom else processAtom a
processExpr (List []) = undefined -- unreachable
processExpr (List (x:xs)) = foldr App (processExpr x) (processExpr <$> xs)

checkName :: AbstSynTree -> Either ParseError Name
checkName (Atom (Ident n)) = Right n
checkName _ = Left NotValidName

checkArgs :: AbstSynTree -> Either ParseError [Name]
checkArgs (Atom (Ident n)) = Right [n]
checkArgs (List xs) = fmap concat $ mapM checkArgs xs
checkArgs _ = Left NotValidArg

processStatement :: [Declaration] -> [AbstSynTree] -> Either ParseError [Declaration]
processStatement decls [] = Right decls
processStatement decls (first:rest) = case first of
    Atom (Ident "define")
        | [name, e] <- rest -> do
            n <- checkName name
            Right $ VarDecl n (processExpr e) : decls
        | otherwise -> Left DefineUnmetArity

    Atom (Ident "lambda")
        | [args, e] <- rest -> do
            a <- checkArgs args
            Right $ FunDecl "" a (processExpr e):decls
        | otherwise -> Left LambdaUnmetArity

    Atom (Ident "defun")
        | [name, args, e] <- rest -> do
            n <- checkName name
            a <- checkArgs args
            Right $ FunDecl n a (processExpr e):decls
        | otherwise -> Left DefunUnmetArity

    _ -> Right []

parse :: [AbstSynTree] -> Either ParseError Program
parse trees = do
    decls <- processStatement [] trees
    let exprs' = processExpr <$> trees
    Right $ Program decls exprs'
