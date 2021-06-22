{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}

module Parser (parse) where

import Z.Data.Vector.Base (Bytes)
import GHC.Generics (Generic)
import Z.Data.Text (Text, Print)
import Data.Foldable (foldl')

import Tokeniser
    ( Atom (Ident, Int, String, Nil)
    , AbstSynTree (Atom, List)
    , parseSexp
    )

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

data Literal = Num Integer | Str Text | Unit
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass Print

newtype Program = Program [WeakTerm] deriving Show

type Name = Text

data WeakTerm
    = WeakTermVar Name
    | WeakTermApp WeakTerm WeakTerm
    | WeakTermLam Name WeakTerm
    | WeakTermLit Literal
    | WeakTermVarDecl Name WeakTerm
    | WeakTermFunDecl Name [Name] WeakTerm
    | WeakTermEnumDecl Name [WeakTerm]
    deriving Show

data ParseError
    = DefineUnmetArity
    | DefunUnmetArity
    | LambdaUnmetArity
    | NotValidName
    | NotValidArg
    | InvalidToken
    | TokeniseError [Text]
    deriving Show

-- NOTE: Enable OverloadedStrings so that Text can be coerced to Bytes in ghci
parse :: Bytes -> Either ParseError [WeakTerm]
parse srcBytes = case parseSexp srcBytes of
    Left err -> Left (TokeniseError err)
    Right as  -> mapM parse' as

parse' :: AbstSynTree -> Either ParseError WeakTerm
parse' (Atom a) = Right (processAtom a)
parse' (List as) = processList as

processAtom :: Atom -> WeakTerm
processAtom (Ident name) = WeakTermVar name
processAtom (Int int)    = WeakTermLit (Num int)
processAtom (String str) = WeakTermLit (Str str)
processAtom Nil          = WeakTermLit Unit

processList :: [AbstSynTree] -> Either ParseError WeakTerm
processList [] = Right (WeakTermLit Unit)
processList (a:as) = if isKeyword a
    then processStatement (a:as)
    else foldl' WeakTermApp <$> parse' a <*> mapM parse' as

isKeyword :: AbstSynTree -> Bool
isKeyword = \case
    Atom (Ident "define") -> True
    Atom (Ident "lambda") -> True
    Atom (Ident "defun")  -> True
    Atom (Ident "enum")   -> True
    _                     -> False

checkName :: AbstSynTree -> Either ParseError Name
checkName (Atom (Ident n)) = Right n
checkName _ = Left NotValidName

checkArgs :: AbstSynTree -> Either ParseError [Name]
checkArgs (Atom (Ident n)) = Right [n]
checkArgs (List xs) = fmap concat $ mapM checkArgs xs
checkArgs _ = Left NotValidArg

processStatement :: [AbstSynTree] -> Either ParseError WeakTerm
processStatement [] = Left InvalidToken
processStatement (first:rest) = case first of
    Atom (Ident "define")
        | [name, expr] <- rest ->
            WeakTermVarDecl <$> checkName name <*> parse' expr
        | otherwise -> Left DefineUnmetArity

    Atom (Ident "lambda")
        | [args, expr] <- rest ->
            WeakTermFunDecl "" <$> checkArgs args <*> parse' expr
        | otherwise -> Left LambdaUnmetArity

    Atom (Ident "defun")
        | [name, args, expr] <- rest -> do
            WeakTermFunDecl <$> checkName name <*> checkArgs args <*> parse' expr
        | otherwise -> Left DefunUnmetArity

    -- TODO: Atom (Ident "enum")

    _ -> Left InvalidToken
