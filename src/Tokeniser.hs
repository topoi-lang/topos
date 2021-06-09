{-# language MagicHash #-}
{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}

module Tokeniser (
    sexp
) where

import Data.Functor (($>))
import GHC.Generics (Generic)
import GHC.Word (Word8)

import Z.Data.Text                   (Text, Print)
import Z.Data.Vector                 (Vector)
import qualified Z.Data.Text.Base    as T
import qualified Z.Data.Parser       as P
import qualified Z.Data.Vector       as V
import Z.Data.JSON.Value (string)

import Z.Data.ASCII

data SExpr
    = Ident  Text     -- | Atom, identifier, potentially keyword
    | Int    Integer  -- | Atom, number literal
    | String Text     -- | Atom, string literal
    | Nil             -- | empty
    | List   (Vector SExpr)
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
sexp :: P.Parser SExpr
sexp = P.word8 PAREN_LEFT *> sexp_
    
sexp_ :: P.Parser SExpr
sexp_ = do
    skipSpaces
    w <- P.peek
    if w == PAREN_RIGHT
        then P.skipWord8 $> Nil
        else List <$> loop [] 1
  where
    loop :: [SExpr] -> Int -> P.Parser (Vector SExpr)
    loop acc !n = do
        !a <- atom
        let acc' = a:acc
        ch <- P.satisfy $ \w -> w == PAREN_RIGHT || ws w
        if ch /= SPACE
            then pure $! V.packRN n acc'  -- it is PAREN_RIGHT, packing this vector
            else do
                skipSpaces
                w <- P.peek
                if w == PAREN_RIGHT
                    then pure $! V.packRN n acc' -- it is also PAREN_RIGHT
                    else loop acc' (n+1)

atom :: P.Parser SExpr
atom = do
    skipSpaces
    w <- P.peek
    case w of
        DOUBLE_QUOTE           -> String <$> string
        PAREN_LEFT             -> P.skipWord8 *> sexp_ 
        _ | w >= 48 && w <= 57 -> Int <$> P.integer
          | not (ws w)         -> Ident <$> identifier
          | otherwise          -> P.fail' "unknown token"

identifier :: P.Parser Text
identifier = T.Text <$> P.takeWhile1 (\w -> isLower w || isUpper w)
