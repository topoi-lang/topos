module TypeCheck where

import Data.Maybe (fromJust)
import Protolude
import Parser

tAtom :: Atom -> Ty
tAtom (AtomList xs) = TyArray . tAtom . fromJust $ headMay xs
tAtom (AtomTuple xs) = TyTuple (fmap tAtom xs)
tAtom (AtomStr _) = TyArray (TyInt U8)
tAtom (AtomLit lit) = case lit of
    IntNumber _ -> TyInt I32
    FloatNumber _ -> TyFloat F32
    Boolean _ -> TyBool -- TODO: until sum type is implemented
