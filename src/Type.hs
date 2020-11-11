-- | Should refine the TypeCheckingMonad's result for better error recovery.
-- | Now it only return Right (returnTy) if it is successfully type-checked.
module Type where

import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap

import Data.HashSet (HashSet)
import qualified Data.HashSet as HSet

import Name
import Term

data TypeCheckError
  = TypeMismatch Type Type
  | NotAFunction Type
  | NotInScope Name
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
typeCheck _ (Lit (LInt _)) = pure TInt
typeCheck _ (Lit (LBool _)) = pure TBool
typeCheck _ (Lit (LString _)) = pure TString
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
    (TInt, TInt) -> pure TInt
    (TInt, y')   -> Left $ TypeMismatch TInt y'
    (x', TInt)   -> Left $ TypeMismatch TInt x'
    (x', y')     -> Left $ TypeMismatch x' y'

---------------------------
-- substitutions
---------------------------
data Scheme = Scheme [Name] Type

type Subst = HashMap Name Type

class Types a where
  -- free type variables
  ftv :: a -> HashSet Name
  -- apply substitution
  apply_subst :: Subst -> a -> a


instance Types Type where
  ftv (TVar name) = HSet.singleton name
  ftv (TInt) = HSet.empty
  ftv (TBool) = HSet.empty
  ftv (TClosure t1 t2) = HSet.union (ftv t1) (ftv t2)

  apply_subst s (TVar name) = fromMaybe (TVar name) (HMap.lookup name s)
  apply_subst s (TClosure arg ret) = TClosure (apply_subst s arg) (apply_subst s ret)
  apply_subst _ t = t


instance Types Scheme where
  ftv (Scheme tvars t) = HSet.difference (ftv t) (HSet.fromList tvars)
  apply_subst s (Scheme tvars t) = Scheme tvars $ apply_subst (foldr HMap.delete s tvars) t