{-# LANGUAGE LambdaCase #-}
-- | Should refine the TypeCheckingMonad's result for better error recovery.
-- | Now it only return Right (returnTy) if it is successfully type-checked.
module Type where

import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap

import Data.HashSet (HashSet)
import qualified Data.HashSet as HSet

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity

import Name
import Term

data TypeCheckError
  = TypeMismatch Type Type
  | NotAFunction Type
  | NotInScope Name
  | UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable Name
  deriving (Show)

type Env = HashMap Name Type

initEnv :: Env
initEnv = HMap.empty

extend :: Name -> Type -> Env -> Env
extend = HMap.insert

lookupName :: Env -> Name -> Type
lookupName env name = fromMaybe
  (error . show $ NotInScope name) (HMap.lookup name env)

typeCheck :: Env -> Term -> Either TypeCheckError Type
typeCheck _ (Lit (LInt _)) = pure tInt
typeCheck _ (Lit (LBool _)) = pure tBool
typeCheck _ (Lit (LString _)) = pure tString
typeCheck env (Var name) = pure $ lookupName env name

typeCheck env (Lam name ty term) = do
  returnTy <- typeCheck (extend name ty env) term
  pure $ TClosure ty returnTy

typeCheck env (App t1 t2) = do
  t1' <- typeCheck env t1
  t2' <- typeCheck env t2
  case t1' of
    TClosure x y | x == t2'  -> return y
                 | otherwise -> error . show $ TypeMismatch x t2'
    ty -> Left $ NotAFunction ty

-- Should refactor this one instead of rebinding
-- | So if we look into the BinOps here and determine which function signature
-- | we want to use, it is considered polymorphic already.
typeCheck env (BinOps _ x y) = do
  x' <- typeCheck env x
  y' <- typeCheck env y
  case (x', y') of
    (TConstructor "Int", TConstructor "Int") -> pure tInt
    (TConstructor "Int", y')   -> Left $ TypeMismatch tInt y'
    (x', TConstructor "Int")   -> Left $ TypeMismatch tInt x'
    (x', y')     -> Left $ TypeMismatch x' y'

---------------------------
-- substitutions
---------------------------

data Scheme = Forall [TVar] Type
newtype TypeEnv = TypeEnv (HashMap Name Scheme)

newtype CountForName = CountForName { count :: Int }

initCount :: CountForName
initCount = CountForName { count = 0 }

tyEnvExtend :: TypeEnv -> (Name, Scheme) -> TypeEnv
tyEnvExtend (TypeEnv env) (x, s) = TypeEnv $ HMap.insert x s env

-- ExceptT e m a = m (Either e a)
type InferM a = ExceptT TypeCheckError (State CountForName) a
type Substitution = HashMap TVar Type

nullSubst :: Substitution
nullSubst = HMap.empty

combineSubst :: Substitution -> Substitution -> Substitution
combineSubst s1 s2 = HMap.map (apply s1) s2 `HMap.union` s1

runInfer :: InferM (Substitution, Type) -> Either TypeCheckError Scheme
runInfer m = case evalState (runExceptT m) initCount of
  Left err -> Left err
  Right res -> Right (closeOver res)

closeOver :: (Substitution, Type) -> Scheme
closeOver (subst, ty) = undefined

class Substitutable a where
  apply :: Substitution -> a -> a
  ftv   :: a -> HashSet TVar

instance Substitutable Type where
  apply _ (TConstructor a) = TConstructor a
  apply s (TClosure t1 t2) = TClosure (apply s t1) (apply s t2)
  apply s t@(TVar a) = HMap.findWithDefault t a s
  
  ftv TConstructor{} = HSet.empty
  ftv (TVar a) = HSet.singleton a
  ftv (TClosure t1 t2) = ftv t1 `HSet.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as (apply s' t) where s' = foldr HMap.delete s as
  ftv (Forall as t) = ftv t `HSet.difference` HSet.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (HSet.union . ftv) HSet.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ HMap.map (apply s) env
  ftv (TypeEnv env) = ftv $ HMap.elems env

freshName :: InferM Type
freshName = do
  countForName <- gets count
  modify (\c -> c { count = countForName + 1 })
  return $ TVar $ TV (letters !! countForName)

letters :: [Name]
letters = [1..] >>= fmap strToName . flip replicateM ['a'..'z']

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck tvar ty = tvar `HSet.member` ftv ty

unify :: Type -> Type -> InferM Substitution
unify (TClosure l r) (TClosure l' r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `combineSubst` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t

unify (TConstructor a) (TConstructor b) | a == b = return nullSubst

unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> InferM Substitution
bind a t | t == TVar a = return nullSubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return $ HMap.singleton a t

-- Instantiation: Converting a σ type into a τ type by creating fresh
-- names for each type variable taht does not appear in the current
-- type environment
instantiate ::  Scheme -> InferM Type
instantiate (Forall as t) = do
  as' <- mapM (const freshName) as
  let s = HMap.fromList $ zip as as'
  return $ apply s t

-- Generalization: Converting a τ type into a σ type by crosing over
-- all free type variabels in a type scheme.
generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
    where as = HSet.toList $ ftv t `HSet.difference` ftv env

lookupEnv :: Name -> TypeEnv -> InferM (Substitution, Type)
lookupEnv name (TypeEnv env) = case HMap.lookup name env of
  Nothing -> throwError $ UnboundVariable name
  Just s  -> do t <- instantiate s
                return (nullSubst, t)

infer :: TypeEnv -> Term -> InferM (Substitution, Type)
infer env expr = case expr of
  Var name -> lookupEnv name env

  Lam name userType expr -> do
    tv <- freshName
    let env' = env `tyEnvExtend` (name, Forall [] tv)
    (s1, t1) <- infer env' expr
    return (s1, apply s1 tv)

  _ -> return (nullSubst, tInt)
  -- Lam x e -> do
  --   tv <- freshName
  --   t <- 




-- closeOver :: (Map.Map TVar Type, Type) -> Scheme
-- closeOver (sub, ty) = normalize sc
--   where sc = generalize emptyTyenv (apply sub ty)

-- Forall [] tInt
-- Forall [TV "a"] TVar "a"
-- forall a. a

-- const :: a -> b -> a
-- Forall [TV "a", TV "b"] TClosure (TVar "a") (TVar "b")
-- generalize :: Type -> Scheme
-- inference stge, we will get the TV "a" and TV "b"

-- Forall [TV "a"] (TClosure (TVar "f") (TVar "a"))

-- forall a. f a

-- ftv (TApp f a) - ["a"]
-- ftv f ++ ftv a - ["a"]
-- [f] ++ [a] - [a]

-- filter (/= a) [f, a]
