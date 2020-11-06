{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Text.Megaparsec
import qualified Text.Megaparsec.Byte as B
import qualified Text.Megaparsec.Byte.Lexer as L

import Data.Char (isAlphaNum, chr)
import Data.ByteString.Internal (ByteString, c2w)
import Data.ByteString (pack)
import GHC.Word (Word8)
import Data.Void (Void)

import Term
import Name (Name, toName)

type Parser = Parsec Void ByteString

ws :: Parser ()
ws = L.space B.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "}-")

lexeme = L.lexeme ws -- so I no need to append `<* spaces` everywhere
symbol s = lexeme (B.string s)
char c = lexeme (B.char . c2w $ c)
parens p = char '(' *> p <* char ')'
braced p = char '{' *> p <* char '}'

-- | Convert a byte to char.
toChar :: Word8 -> Char
toChar = chr . fromIntegral
{-# INLINE toChar #-}

keyword :: ByteString -> Bool
keyword x = x == "let" || x == "in"

pIdentifier :: Parser Name
pIdentifier = do
  x <- takeWhile1P Nothing (isAlphaNum . toChar)
  toName x <$ ws

pBind :: Parser Name
pBind = pIdentifier <|> (toName <$> symbol "_")

pString :: Parser ByteString
pString = pack <$> (char '\"' *> manyTill anySingle (char '\"'))

pNumber :: Parser Int
pNumber = lexeme L.decimal

pAtomicLit :: Parser Lit
pAtomicLit = choice [ LInt <$> pNumber
                   , LBool True <$ symbol "true"
                   , LBool False <$ symbol "false"
                   , LString <$> pString
                   ]

atomicTerm :: Parser Term
atomicTerm = (Var <$> pIdentifier)
         <|> (Lit <$> pAtomicLit)
         <|> parens atomicTerm

typeParse :: Parser Type
typeParse = choice
  [ TInt <$ symbol "Int"
  , TBool <$ symbol "Bool"
  ]

bindingWithTypeAnnotation :: Parser (Name, Type)
bindingWithTypeAnnotation = parens ((,) <$> pBind <*> (char ':' *> typeParse))

pLambda :: Parser Term
pLambda = do
  char '\\'
  bindingsWType <- some bindingWithTypeAnnotation
  char '.'
  term <- atomicTerm
  pure $ foldr (\(name, ty) term -> Lam name ty term) term bindingsWType

-- entrypoint
termParser :: Parser Term
termParser = ws *> pLambda <* eof

parseSrc :: String -> ByteString -> Either String Term
parseSrc filename src = case parse termParser filename src of
  Left e -> Left $ errorBundlePretty e
  Right t -> Right t
