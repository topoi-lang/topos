{-# language LambdaCase #-}

module TypeCheck (typeCheckWeakTerm) where

import Parser (Literal(..), WeakTerm(..), Name, TypeLiteral(..))

import Z.Data.Vector.FlatMap (FlatMap)
import qualified Z.Data.Vector.FlatMap as FlatMap

import Data.Foldable (foldlM)

data TypeCheckError
    = NotAFunction
    | VarUndefined Name
    | VarDefined Name
    deriving Show

type TypeContext = FlatMap Name TypeLiteral

initContext :: TypeContext
initContext = FlatMap.empty

inferLiteralType :: Literal -> TypeLiteral
inferLiteralType = \case
    Num _ -> TyInt
    Str _ -> TyStr
    Unit  -> TyBottom

-- TODO: can actually concat those typeCheckError
typeCheckWeakTerm :: [WeakTerm] -> Either TypeCheckError (TypeContext, TypeLiteral)
typeCheckWeakTerm = foldlM typeCheck (initContext, TyBottom)

typeCheck :: (TypeContext, TypeLiteral) -> WeakTerm -> Either TypeCheckError (TypeContext, TypeLiteral)
typeCheck (ctxt, type') = \case
    WeakTermVar name -> case FlatMap.lookup name ctxt of
      Nothing -> Left (VarUndefined name)
      Just ty -> Right (ctxt, ty)

    WeakTermApp{} -> undefined
    WeakTermLam{} -> undefined

    WeakTermLit lit -> Right (ctxt, inferLiteralType lit)

    WeakTermVarDecl name term -> do
        (_, tyTerm) <- typeCheck (ctxt, type') term
        Right (FlatMap.insert name tyTerm ctxt, TyBottom)

    WeakTermFunDecl{} -> undefined

    WeakTermEnumDecl{} -> undefined

    WeakTermTypeDecl name ty -> Right (FlatMap.insert name ty ctxt, TyBottom)
    
-- typeCheckWeakTerm $ fromRight [WeakTermLit Unit] $ parse "(type int Int)(int)(in)"