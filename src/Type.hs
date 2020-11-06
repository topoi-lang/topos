-- | Should refine the TypeCheckingMonad's result for better error recovery.
-- | Now it only return Right (returnTy) if it is successfully type-checked.
module Type where

import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap

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
