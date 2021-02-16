module Parser where

import Data.Char

import FlatParse hiding (Parser, runParser, char, string, err, switch)
import qualified FlatParse as FP
import Syntax
import Lexer

-- Identifier -------------------------------------------------------------------------------------
identStartChar :: Parser ()
identStartChar =
  () <$ satisfy' isLatinLetter isGreekLetter isLetter isLetter
{-# inline identStartChar #-}

identChar :: Parser ()
identChar =
  () <$ satisfy' (\c -> isLatinLetter c || FlatParse.isDigit c) isGreekLetter isAlphaNum isAlphaNum

inlineIdentChar :: Parser ()
inlineIdentChar =
  () <$ satisfy' (\c -> isLatinLetter c || FlatParse.isDigit c) isGreekLetter isAlphaNum isAlphaNum
{-# inline inlineIdentChar #-}

manyIdents :: Parser ()
manyIdents = many_ inlineIdentChar

skipToSpan :: Pos -> Parser Span
skipToSpan l = br identChar
  (do {manyIdents; r <- getPos; ws; pure (Span l r)})
  empty
{-# inline skipToSpan #-}

-- This parses only the indentifier, will skip the builtin keyword
pIdent :: Parser Span
pIdent = do
  checkIndent
  l <- getPos
  $(FP.switch [| case _ of
    "let"     -> skipToSpan l
    "in"      -> skipToSpan l
    _         -> do {identStartChar; manyIdents; r <- getPos; ws; pure (Span l r)} |])

pMinus = $(char '-')
pComma = $(char ',')
pDot = $(char '.')
pColon = $(char ':')
pParenL = $(char '(')
pParenR = $(char ')')
pBraceL = $(char '{')
pBraceR = $(char '}')
pBracketL = $(char '[')
pbracketR = $(char ']')
pArrow = $(string "->")
pIn = $(string "in")
pAssign = $(char '=')

pNat = $(string "Nat")

-- Parser combinators ---------------------------------------------------------

skipToVar :: Pos -> (Pos -> Parser Expr) -> Parser Expr
skipToVar l p = br identChar
  (do { manyIdents; r <- getPos; ws; pure $ Var (Span l r) })
  (do { r <- getPos; ws; p r })
{-# inline skipToVar #-}

pAtomicExpr :: Parser Expr
pAtomicExpr = do
  checkIndent
  l <- getPos
  $(FP.switch [| case _ of
    -- "_" -> do { r <- getPos; ws; pure $ Hole (Span l r) }
    -- "(" -> ws *> pTm <* pParenR
    "let" -> skipToVar l $ \_ -> empty
    "in" -> skipToVar l $ \_ -> empty
    "Nat" -> skipToVar l $ \_ -> empty
    _ -> do { identChar; manyIdents; r <- getPos; ws; pure $ Var (Span l r) } |])

pType :: Parser Type
pType = do
  pos <- getPos
  t <- TNat <$ pNat -- TODO: now only support TNat
  br pArrow
    (TArrow pos t <$> pType)
    (pure t)

pLet :: Pos -> Parser Expr
pLet pos = do
  x <- pIdent <?> "expected an identifier"
  $(switch [| case _ of
    "=" -> do
      t <- pAtomicExpr
      pIn <?> "expected \"in\" in let expression"
      u <- pAtomicExpr
      pure $ Let pos x Nothing t u
    ":" -> do
      a <- pType
      pAssign <?> "expected \"=\" in let expression"
      t <- pAtomicExpr
      pIn <?> "expected \"in\" in let expression"
      u <- pAtomicExpr
      pure $ Let pos x (Just a) t u
    _ -> err "expected \":\" or \"=\" in let expression" |])

pLamLetExp :: Parser Expr
pLamLetExp = do
  checkIndent
  l <- getPos
  $(FP.switch [| case _ of
    -- where is Lam ?
    "let" -> skipToVar l $ \_ -> pLet l
    _ -> ws >> pAtomicExpr |])

pAppExp :: Parser Expr
pAppExp = checkIndent >> foldl1 App <$> some pAtomicExpr

pSrc :: Parser Expr
pSrc = ws *> pAppExp <|> pLamLetExp <* eof