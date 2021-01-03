module Parser where

import Protolude hiding (try, some)
import GHC.Base (String)
import Data.Char
import Data.Maybe (fromJust)

import qualified Data.ByteString as BS
import Text.Megaparsec
import qualified Text.Megaparsec.Byte as B
import qualified Text.Megaparsec.Byte.Lexer as L

import Data.ByteString.Internal (c2w, w2c)

data Identifier
  = Hole
  | Id ByteString 
  | UnderscoreId ByteString  -- Id starts with an underscore, eg "_a"
  deriving (Show)

data IntType
  = I8 | I16 | I32 | I64
  | U8 | U16 | U32 | U64
  deriving (Show)

data FloatType = F32 | F64 deriving (Show)

data Ty
  = TyInt IntType
  | TyFloat FloatType
  | TyTuple [Ty]
  | TyArray Ty -- (Maybe Int)
  | TyFunction { arg :: Ty, ret :: Ty }
  deriving (Show)
  -- sum type
  -- record type
  -- Polytype?

data Declaration
  = DeclFunctionBind { name :: Identifier, args :: [Identifier], expr :: Expr }
  | DeclValBind Identifier Expr
  | DeclTyBind Identifier Ty
  deriving (Show)

type Parser = Parsec Void ByteString

ws :: Parser ()
ws = L.space B.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "}-")

lexeme = L.lexeme ws
symbol = lexeme . B.string
char c = lexeme (B.char . c2w $ c)
parens = between (char '(') (char ')')
braced = between (char '{') (char '}')
brackets = between (char '[') (char ']')
space = char ' '

pStringLiteral :: Parser ByteString
pStringLiteral = BS.pack <$> (char '\"' *> manyTill anySingle (char '\"'))

-- | Convert a byte to char
toChar :: Word8 -> Char
toChar = chr . fromIntegral
{-# INLINE toChar #-}

pIdentifier :: Parser Identifier
pIdentifier = ident >>= validate
  where
    ident = do
      x <- takeWhile1P Nothing (isAlphaNum . toChar)
      x <$ ws

    validate x = pure $ case (BS.length x, w2c $ BS.head x) of
      (1, '_') -> Hole
      (_, '_') -> UnderscoreId x
      (_,   _) -> Id x

pPrimTy :: Parser Ty
pPrimTy = choice
  [ TyInt I8  <$ "i8"
  , TyInt I16 <$ "i16"
  , TyInt I32 <$ "i32"
  , TyInt I64 <$ "i64"
  , TyInt U8  <$ "u8"
  , TyInt U16 <$ "u16"
  , TyInt U32 <$ "u32"
  , TyInt U64 <$ "u64"
  , TyFloat F32 <$ "f32"
  , TyFloat F64 <$ "f64"
  , tupleParse pTy >>= \ts -> case unwrapSingleTuple ts of
      Left ty -> pure ty
      Right t -> pure $ TyTuple t
  , TyArray <$> brackets pPrimTy
  , parens pTy
  ]

tupleParse p = parens $ sepBy1 p (char ',')
arrayParse p = brackets $ sepBy1 p (char ',')

unwrapSingleTuple :: [a] -> Either a [a]
unwrapSingleTuple xs = case length xs of
  1 -> Left . fromJust $ headMay xs
  _ -> Right xs

pTy :: Parser Ty
pTy = pPrimTy `chainr1` arrow
  where
    -- should write a tokenizer so I don't need to deal with spaces anymore
    arrow = ws *> symbol "->" $> TyFunction

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where
    rest a = (do f <- op
                 b <- p
                 rest (f a b))
             <|> return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = scan
  where
    scan = p >>= rest
    rest a = do f <- op
                b <- scan
                return (f a b)
             <|> return a

pTypeBind :: Parser Declaration
pTypeBind = do
  name <- pIdentifier
  DeclTyBind name <$> (char ':' *> pTy)

data Literal
  = IntNumber Int
  | FloatNumber Double
  | Boolean Bool
  deriving (Show)

pLiteral :: Parser Literal
pLiteral = choice
  [ IntNumber <$> L.decimal
  , FloatNumber <$> L.float
  , Boolean False <$ symbol "false"
  , Boolean True <$ symbol "true"
  ]

data Atom
  = AtomLit Literal
  | AtomStr ByteString
  | AtomList [Atom]
  | AtomTuple [Atom]
  deriving (Show)

pAtom :: Parser Atom
pAtom = choice
  [ AtomLit <$> pLiteral
  , AtomStr <$> pStringLiteral
  , AtomList <$> arrayParse pAtom
  , tupleParse pAtom >>= \as -> case unwrapSingleTuple as of
    Left a -> pure a
    Right a -> pure $ AtomTuple a
  , parens pAtom
  ]

data Expr
  = ExprAtom Atom
  | ExprApp Expr Expr
  | ExprLet { _pat :: Identifier, _expr :: Expr, _body :: Expr } -- no type annotation ?
  | ExprVar ByteString
  -- lambdas where ?
  -- if then else ?
  deriving (Show)

pLet :: Parser Expr
pLet = do
  symbol "let"
  x <- pIdentifier -- should be pattern binder
  symbol "="
  t <- pExpr
  symbol "in"
  u <- pExpr
  pure $ ExprLet x t u

pApp :: Parser Expr
pApp = (ExprVar <$> do takeWhile1P Nothing (isAlphaNum . toChar)) `chainl1` (char ' ' $> ExprApp)

pExpr :: Parser Expr
pExpr = choice
  [ ExprAtom <$> pAtom
  , pApp
  , pLet
  ]

pFuncBind :: Parser Declaration
pFuncBind = do
  name <- pIdentifier
  args <- some pIdentifier
  symbol "="
  expr <- pExpr
  pure $ DeclFunctionBind name args expr

pValBind :: Parser Declaration
pValBind = do
  name <- pIdentifier
  symbol "="
  expr <- pExpr
  pure $ DeclValBind name expr

pDecl :: Parser Declaration
pDecl = try pValBind <|> try pFuncBind <|> pTypeBind

parseSource :: String -> ByteString -> Either String Declaration
parseSource filename src =
  case parse pDecl filename src of
    Left e -> Left $ errorBundlePretty e
    Right decl -> Right decl