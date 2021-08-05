{-# language LambdaCase #-}

module TypeCheck (inferLiteralType) where

import qualified Parser (Literal(..))

data Type
  = Number
  | String
  | Nil -- Void

inferLiteralType :: Parser.Literal -> Type
inferLiteralType = \case
  Parser.Num _ -> Number
  Parser.Str _ -> String
  Parser.Unit  -> Nil
