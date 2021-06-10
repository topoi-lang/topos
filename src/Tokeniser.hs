{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}

module Tokeniser (
    sexp, Atom(..), Term(..)
) where

import Data.Functor (($>))
import GHC.Generics (Generic)
import GHC.Word (Word8)

import Z.Data.Text                   (Text, Print)
import qualified Z.Data.Text.Base    as T
import qualified Z.Data.Parser       as P
import Z.Data.JSON.Value (string)

import Z.Data.ASCII

data Atom
    = Ident  Text     -- | Atom, identifier, potentially keyword
    | Int    Integer  -- | Atom, number literal
    | String Text     -- | Atom, string literal
    | Nil             -- | empty
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass Print

data Term
    = Atom    Atom
    | List    [Term]
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass Print

-- | skip all white spaces like tab, newline and carriage returns
skipSpaces :: P.Parser ()
skipSpaces = P.skipWhile ws
{-# INLINABLE skipSpaces #-}

-- parseSExp :: Text -> Either P.ParseError SExpr
-- parseSExp = P.parse' (sexp <* skipSpaces <* P.endOfInput)

ws :: Word8 -> Bool
ws w = w == TAB || w == SPACE || w == NEWLINE || w == CARRIAGE_RETURN
{-# inline ws #-}

-- | parse S expression with leading left parenthesis
sexp :: P.Parser Term
sexp = P.word8 PAREN_LEFT *> sexp_
    
sexp_ :: P.Parser Term
sexp_ = do
    skipSpaces
    w <- P.peek
    if w == PAREN_RIGHT
        then P.skipWord8 $> Atom Nil
        else List <$> loop []
  where
    loop :: [Term] -> P.Parser [Term]
    loop acc = do
        !a <- atom
        let acc' = a:acc
        ch <- P.satisfy $ \w -> w == PAREN_RIGHT || ws w
        if ch /= SPACE
            then pure $! reverse acc'  -- it is PAREN_RIGHT, packing this vector
            else do
                skipSpaces
                w <- P.peek
                if w == PAREN_RIGHT
                    then pure $! reverse acc' -- it is also PAREN_RIGHT
                    else loop acc'

atom :: P.Parser Term
atom = do
    skipSpaces
    w <- P.peek
    case w of
        DOUBLE_QUOTE           -> Atom . String <$> string
        PAREN_LEFT             -> P.skipWord8 *> sexp_ 
        _ | w >= 48 && w <= 57 -> Atom . Int <$> P.integer
          | not (ws w)         -> Atom . Ident <$> identifier
          | otherwise          -> P.fail' "unknown token"

identifier :: P.Parser Text
identifier = T.Text <$> P.takeWhile1 (\w -> isLower w || isUpper w)
