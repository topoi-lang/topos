{-# OPTIONS_GHC -funbox-strict-fields #-}
module AST where

import FlatParse
import Data.List
import Control.Monad.Except

-- data Tm
--   = Var Span
--   | Let Pos Span (Maybe Tm) {- type annotation -} Tm Tm
--   | App Tm Tm
--   | Hole Span
--   deriving Show

-- | Reference: http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html
type Sym = String
data Expr
  = Var String
  | App Expr Expr
  | Lam Sym Type Expr
  deriving (Eq, Read, Show)

whnf :: Expr -> Expr
whnf ee = spine ee []
  where
    spine (App f a) as = spine f (a:as)
    spine (Lam s t e) [] = Lam s t (whnf e)
    spine (Lam s _ e) (a:as) = spine (subst s a e) as
    spine f as = app f as
    app f as = foldl App f (map whnf as)

freeVars :: Expr -> [Sym]
freeVars (Var s)     = [s]
freeVars (App f a)   = freeVars f `union` freeVars a
freeVars (Lam s _ e) = freeVars e \\ [s]

-- b[v:=x], replace all v by b inside x
subst :: Sym -> Expr -> Expr -> Expr
subst v x b = sub b
  where
    sub e@(Var i) = if i == v then x else e
    sub (App f a) = App (sub f) (sub a)
    sub (Lam i t e) | v == i = Lam i t e
                    | i `elem` freeVars x = let i' = cloneSym e i
                                                e' = substVar i i' e
                                            in Lam i' t (sub e')
                    | otherwise  = Lam i t (sub e)

    cloneSym e i = go i
      where
        go i' = if i' `elem` vars then go (i ++ "'") else i'
        vars = freeVars x ++ freeVars e

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var v) (Var v') = v == v'
alphaEq (App f a) (App f' a') = alphaEq f f' && alphaEq a a'
alphaEq (Lam s _ e) (Lam s' _ e') = alphaEq e (substVar s' s e')
alphaEq _ _ = False

betaEq :: Expr -> Expr -> Bool
betaEq e e' = alphaEq (whnf e) (whnf e')

--- Simply typed lambda calculus
data Type = Base | Arrow Type Type -- So arrow is function, t -> t B
  deriving (Eq, Read, Show)

-- All the function we had for the untyped lambda calculus can be trivially extended
-- to the simply typed one by simply carrying the type along.
newtype Env = Env [(Sym, Type)] deriving Show

initEnv :: Env
initEnv = Env []

extend :: Sym -> Type -> Env -> Env
extend s t (Env r) = Env ((s, t) : r)

-- Type checking can go wrong; here is the type errors
type ErrorMsg = String

type TyChM a = Either ErrorMsg a

lookupVar :: Env -> Sym -> TyChM Type
lookupVar (Env r) s = case lookup s r of
  Just t  -> return t
  Nothing -> throwError $ "Cannot find variable " ++ s

tyCheck :: Env -> Expr -> TyChM Type
tyCheck r (Var s) = lookupVar r s
tyCheck r (App f a) = do
  typeof_f <- tyCheck r f
  case typeof_f of
    Arrow typeof_a typeof_r -> do
      typeof_a' <- tyCheck r a
      when (typeof_a /= typeof_a') $
        throwError "Bad function argument type"
      return typeof_r
    _ -> throwError "Applied non-function"
tyCheck r (Lam s t e) = do
  typeof_e <- tyCheck (extend s t r) e
  return $ Arrow t typeof_e

typeCheck :: Expr -> Type
typeCheck e = case tyCheck initEnv e of
  Left msg -> error ("Type error: \n" ++ msg)
  Right t  -> t

app2 :: Expr -> Expr -> Expr -> Expr
app2 f x y = App (App f x) y

-- succ' :: Expr
-- succ' = Lam "n" Base . Lam "s" Base $ Lam "z" Base (App s $ app2 n s z)
--   where
--     [z,s,n] = map (Var . (:[])) "zsn"