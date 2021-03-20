module BidirTypeCheck where

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Hashable
{-
  Haskell implementation of Dunfield and Krishnaswami's "Complete and easy
  bidirectional typechecking for higher-rank polymorphism".

  and this [ollef's Bidirectional](https://github.com/ollef/Bidirectional)


  - need to read purescript compiler as we awnt to do the row-polymorphism,
    making the label become first class.
-}

newtype TVar = TypeVar String deriving (Eq, Ord, Show)

instance Hashable TVar where
  hashWithSalt salt (TypeVar str) = hashWithSalt salt str

data TypeKind = Mono | Poly

data Type :: TypeKind -> * where
  TUnit :: Type a -- (), something like a base?
  TVar :: TVar -> Type a
  TExists :: TVar -> Type a
  TForall :: TVar ->  Type Poly -> Type Poly
  TFun :: Type a -> Type a -> Type a -- or type arrow ?
deriving instance Show (Type a)
deriving instance Eq (Type a)

type Polytype = Type Poly
type Monotype = Type Mono

monotype :: Type a -> Maybe Monotype
monotype = \case
  TUnit -> Just TUnit
  TVar v -> Just (TVar v)
  TForall _ _ -> Nothing
  TExists v   -> Just (TExists v)
  TFun t1 t2  -> TFun <$> monotype t1 <*> monotype t2

polytype :: Type a -> Polytype
polytype = \case
  TUnit       -> TUnit
  TVar v      -> TVar v
  TForall v t -> TForall v t
  TExists v   -> TExists v
  TFun t1 t2  -> TFun (polytype t1) (polytype t2)

freeTVars :: Type a -> HashSet TVar
freeTVars = \case
  TUnit       -> Set.empty
  TVar v      -> Set.singleton v
  TForall v t -> Set.delete v (freeTVars t)
  TExists v   -> Set.singleton v
  TFun t1 t2  -> freeTVars t1 <> freeTVars t2

-- type level substitution like
-- typeSubst A \alpha B = [A/alpha]B
typeSubst :: Type a -> TVar -> Type a -> Type a
typeSubst t' v = \case
  TUnit -> TUnit
  TVar v'      | v' == v   -> t'
               | otherwise -> TVar v'
  TForall v' t | v' == v   -> TForall v' t
               | otherwise -> TForall v' (typeSubst t' v t)
  TExists v'   | v' == v   -> t'
               | otherwise -> TExists v'
  TFun t1 t2               -> TFun (typeSubst t' v t1) (typeSubst t' v t2)

-- TODO: Context Environment ?