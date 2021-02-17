module TypeCheck where

import Control.Monad.Except
import Syntax

import qualified Data.ByteString as B
import FlatParse (span2ByteString, Span(Span))

{-
  File mnemonics:
    - The variable name `xt` means type of x
-}

data TypeError
  = TypeMismatch Type Type
  | UnknownType
  | UnknownVar Sym
  | AppliedNonFunction
  deriving Show

type TypeCheckM a = Except TypeError a
type Sym = B.ByteString
type SourceUnit = B.ByteString

-- NOTE: I remembered there was a conference recording comparing which algorithm/data structure
-- is better in which scenario. I have no idea is this the best data structure to represent
-- scope environment or not.
newtype Env = Env [(Sym, Type)]
  deriving Show

initEnv :: Env
initEnv = Env []

extend :: Sym -> Type -> Env -> Env
extend s t (Env r) = Env ((s, t) : r)

lookupVar :: Env -> Sym -> TypeCheckM Type
lookupVar (Env r) s = case lookup s r of
  Just t  -> pure t
  Nothing -> throwError (UnknownVar s)

typeCheck :: SourceUnit -> Env -> Expr -> TypeCheckM Type
typeCheck src r (Var s) = lookupVar r (span2ByteString src s)
typeCheck _ _ (T _) = pure TBool
typeCheck _ _ (F _) = pure TBool
typeCheck _ _ (Zero _) = pure TNat
typeCheck src r (Succ a) = do
  at <- typeCheck src r a
  case at of
    TNat -> pure TNat
    _    -> throwError (TypeMismatch at TNat)

typeCheck src r (Pred a) = do
  at <- typeCheck src r a
  case at of
    TNat -> pure TNat
    _    -> throwError (TypeMismatch at TNat)

typeCheck src r (IsZero a) = do
  at <- typeCheck src r a
  case at of
    TNat -> pure TBool
    _    -> throwError (TypeMismatch at TNat)

typeCheck src r (App f a) = do
  ft <- typeCheck src r f
  case ft of
    TArrow _ at_userDefined rt -> do
      at <- typeCheck src r a
      when (at /= at_userDefined) $ throwError (TypeMismatch at at_userDefined)
      return rt
    _ -> throwError AppliedNonFunction

typeCheck src r (Lam s@(Span l _) t e) = do
  let sym = span2ByteString src s
  TArrow l t <$> typeCheck src (extend sym t r) e

typeCheck src r (Let _ s (Just at) _term u) = do
  let sym = span2ByteString src s
  typeCheck src (extend sym at r) u

-- TODO: leave it to type inference
typeCheck _ _ (Let _ _ Nothing _ _) = undefined

runTypeCheck :: SourceUnit -> Expr -> Type
runTypeCheck src e = case runExcept $ typeCheck src initEnv e of
  Left msg -> error . show $ msg
  Right t  -> t
