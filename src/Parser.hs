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
    , Term (Atom, List)
    )

type Name = Text
type Scope = FlatMap Name Expr

-- SKI combinator
data Expr
    = Var Name
    | Lit Atom
    | App Expr Expr
    | Lam Name Expr
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass Print

data ParseError
    = NotFunction Atom [Term]
    | UnknownIdentifier Atom
    deriving (Show, Generic)
    deriving anyclass Print

--instance Show ParseError where
--    show (NotFunction atom args) = show . B.buildText $ do
--        "atom" >> show atom >> " is a value, cannot apply " >> foldr (\x accum -> B.text x >> " " >> accum) (B.text "") args >> "to it."

-- | This function only read scope and maps `Atom` to `Expr` node
processAtom :: Scope -> Atom -> Either ParseError Expr
processAtom _s int@Int{}    = Right (Lit int)
processAtom _s str@String{} = Right (Lit str)
processAtom _s Nil          = Right (Lit Nil)
processAtom s ident@(Ident name) = case M.lookup name s of
    Just expr -> Right expr
    Nothing -> Left (UnknownIdentifier ident)

-- | Make nested list become canonical, say, it maps `List [List [Atom Nil]]` to `Atom Nil`
flattenTermList :: Term -> Term
flattenTermList a@Atom{} = a
flattenTermList (List []) = Atom Nil
flattenTermList (List term:rest) = List $ (flattenTermList term) : (flattenTermList <$> rest)

defaultScope :: FlatMap k v
defaultScope = M.empty

processTerm :: Scope -> Term -> Either ParseError (Scope, Expr)
processTerm s term = processTerm_ s (flattenTermList term)

-- | Process term without flatten the nested list
processTerm_ :: Scope -> Term -> Either ParseError (Scope, Expr)
processTerm_ s (Atom a) = case processAtom s a of
    Left err -> Left err
    Right expr -> Right (s, expr)

{-
processTerm_ s (List (term:rest)) = do
    case processTerm_ s term of
        Left err -> Left err
        Right (s, expr) -> if rest == []
            then Right (s, expr)
            else Right (s, App expr (processTerm_ s rest)) -- foldr
-}

-- processTerm defaultScope $ flattenTermList $ fromRight (Atom Nil) $ P.parse' sexp "(a)"