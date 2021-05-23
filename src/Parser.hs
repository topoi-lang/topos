{-# LANGUAGE StrictData #-}
module Parser where

import Data.Char (ord)

import FlatParse.Stateful hiding (Parser, runParser, string, char, cut)
import qualified FlatParse.Stateful as FP

import qualified Data.SpannedTerm as Spanned
import Lexer

-- TODO: support Hole '_'
-- TODO: support different bind type

{-
  NOTE:
  
  - although this is the parser code, if you want to expand builtin
    keywords then you have to go Lexer.hs, follow isKeyword

  - code convention, all the functions ends with `'` are throwing
    unrecovable error on failure, and should provide an error message
-}

-- |  Identifiers
pIdent :: Parser Span
pIdent = do
  checkIndent
  spanOf $ spanned
    (identStartChar *> many_ identChar)
    (\_ span -> fails (isKeyword span))

pIdent' :: Parser Span
pIdent' = pIdent `cutPrecise` Msg "identifier"

-- pNum :: Parser Span
pNum = undefined --satisfyASCII_ isDigit

pAtomExpr :: Parser Spanned.Tm
pAtomExpr = checkIndent >> token $(switch [|
  case _ of
    "_" -> pure Spanned.Hole
    "(" -> ws *> pAtomExpr <* $(symbol ")") `cutPrecise` Lit ")"
    _ -> Spanned.Var <$> pIdent
  |])

pAtomExpr' :: Parser Spanned.Tm
pAtomExpr' = pAtomExpr `cut`
  [Msg "identifier", Msg "parenthesized expression"]

app :: Parser Spanned.Tm
app = checkIndent >> chainl Spanned.App pAtomExpr' pAtomExpr

pType :: Parser Spanned.Type
pType = do
  pos <- getPos
  t <- token $(switch [|
    case _ of
      "Bool" -> pure Spanned.TBool
      "Nat"  -> pure Spanned.TNat
      _ -> Spanned.TCon <$> pIdent
    |])
  branch $(symbol "->")
    (Spanned.TArrow pos t <$> pType)
    (pure t)

pType' :: Parser Spanned.Type
pType' = pType `cutPrecise` Msg "type"

pLetLam :: Parser Spanned.Tm
pLetLam = do
  checkIndent
  pos <- getPos
  token $(switch [| case _ of
    "\\" -> ws >> pLam pos
    _ -> ws >> app |])

pLam :: Pos -> Parser Spanned.Tm
pLam pos = do
  s <- pIdent `cut` [Msg "argument", Msg "binder"]
  token $(switch [| case _ of
    "." -> do
      e <- pLetLam
      pure (Spanned.Lam pos s Nothing e)
    ":" -> do
      t <- pType'
      $(symbol' ".")
      e <- pLetLam
      pure (Spanned.Lam pos s (Just t) e) |]) `cut` [Msg "->", Msg ":"]

pSrc :: Parser Spanned.Tm
pSrc = ws *> app <* eof