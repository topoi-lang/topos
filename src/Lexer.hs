module Lexer where

import FlatParse.Stateful hiding (Parser, runParser, string, char, cut)
import qualified FlatParse.Stateful as FP

import Language.Haskell.TH
import qualified Data.ByteString as B

data Expected
  = Msg String -- ^ An error message
  | Lit String -- ^ Expected thing
  deriving (Eq, Show, Ord)

-- | A parsing error.
data LexerError
  = Precise Pos Expected     -- ^ A precisely known error, like leaving out "in" from "let".
  | Imprecise Pos [Expected] -- ^ An imprecise error, when we expect a number of different things,
                             --   but parse something else.
  deriving Show

type Parser = FP.Parser Int LexerError

merge :: LexerError -> LexerError -> LexerError
merge e e' = case (lexerErrorPos e, lexerErrorPos e') of
  (p, p') | p < p' -> e'
  (p, p') | p > p' -> e
  (p, p') -> case (e, e') of
    (Precise{} , _) -> e
    (_, Precise{}) -> e'
    (Imprecise _ es, Imprecise _ es') -> Imprecise p (es ++ es')
{-# NOINLINE merge #-} -- merge is "cold" code, so we shouldn't inlint it

lexerErrorPos :: LexerError -> Pos
lexerErrorPos (Precise p _) = p
lexerErrorPos (Imprecise p _) = p

cut :: Parser a -> [Expected] -> Parser a
cut p es = do
  pos <- getPos
  FP.cutting p (Imprecise pos es) merge

cutPrecise :: Parser a -> Expected -> Parser a
cutPrecise p e = do
  pos <- getPos
  FP.cutting p (Precise pos e) merge

runParser :: Parser a -> B.ByteString -> Result LexerError a
runParser p = FP.runParser p 0 0

lineComment :: Parser ()
lineComment = 
  optioned anyWord8
    (\case 10 -> ws -- don't worry, it is an ASCII '\n' char
           _  -> lineComment)
    (pure ())

-- | Parse a potentially nested multiline comment.
multilineComment :: Parser ()
multilineComment = go (1 :: Int) where
  go 0 = ws
  go n = $(switch [| case _ of
    "-}" -> go (n - 1)
    "{-" -> go (n + 1)
    _    -> branch anyWord8 (go n) (pure ()) |])

-- | Consume whitespace.
ws :: Parser ()
ws = $(switch [| case _ of
  " "  -> ws
  "\n" -> ws
  "\t" -> ws
  "\r" -> ws
  "--" -> lineComment
  "{-" -> multilineComment
  _    -> pure () |])

-- | Consume whitespace after running a parser.
token :: Parser a -> Parser a
token p = p <* ws
{-# inline token #-}

-- | Read a starting character of an identifier.
identStartChar :: Parser Char
identStartChar = satisfyASCII isLatinLetter
{-# inline identStartChar #-}

-- | Read a non-starting character of an identifier.
identChar :: Parser Char
identChar = satisfyASCII (\c -> isLatinLetter c || isDigit c)
{-# inline identChar #-}

-- | Check whether a `Span` contains exactly a keyword. Does not change parsing state.
isKeyword :: Span -> Parser ()
isKeyword span = inSpan span $ do
  $(FP.switch [| case _ of
      "lam"   -> pure ()
      "let"   -> pure ()
      "in"    -> pure ()
      "\\"    -> pure ()
      "if"    -> pure ()
      "then"  -> pure ()
      "else"  -> pure ()
      "true"  -> pure ()
      "false" -> pure ()  |])
  eof

-- | Parse a non-keyword string.
symbol :: String -> Q Exp
symbol str = [| token $(FP.string str) |]

-- | Parser a non-keyword string, throw precise error on failure.
symbol' :: String -> Q Exp
symbol' str = [| $(symbol str) `cutPrecise` Lit str |]

-- | Parse a keyword string.
keyword :: String -> Q Exp
keyword str = [| token ($(FP.string str) `notFollowedBy` identChar) |]

-- | Parse a keyword string, throw precise error on failure.
keyword' :: String -> Q Exp
keyword' str = [| $(keyword str) `cut'` Lit str |]

-- | A Monad guard to check indentation level.
checkIndent :: Parser ()
checkIndent = do
  lvl <- ask
  currentLvl <- get
  if currentLvl < lvl
    then empty
    else pure ()
