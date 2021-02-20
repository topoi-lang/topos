{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fobject-code #-}
module Lexer where

import FlatParse hiding (Parser)
import qualified FlatParse
import Language.Haskell.TH
import qualified Data.ByteString as B

data ParseError = ParseError Pos String deriving Show
type Parser = FlatParse.Parser Int ParseError

runParser :: Parser a -> B.ByteString -> Result ParseError a
runParser p = FlatParse.runParser p 0 0

ws :: Parser ()
ws = $(FlatParse.switch [| case _ of
  " " -> modify (+1) >> ws
  "\n" -> put 0 >> ws
  "\t" -> modify (+1) >> ws
  "\r" -> modify (+1) >> ws
  "--" -> lineComment
  _ -> pure ()
  |])

lineComment :: Parser ()
lineComment =
  br $(FlatParse.char '\n') (put 0 >> ws) $
  br anyChar_ (modify (+1) >> lineComment) (pure ())

checkIndent :: Parser ()
checkIndent = do
  lvl <- ask
  currentLvl <- get
  if currentLvl < lvl
    then empty
    else pure ()
{-# inline checkIndent #-}

lexeme :: Parser a -> Parser a
lexeme p = checkIndent *> p <* ws
{-# inline lexeme #-}

char :: Char -> Q Exp
char c = [| lexeme $(FlatParse.char c) |]

string :: String -> Q Exp
string str = [| lexeme $(FlatParse.string str) |]

switch :: Q Exp -> Q Exp
switch exp = [| do
  checkIndent
  $(FlatParse.switch' (Just [| ws |]) exp) |]

-- | Label an error message to the `ParseError`.
(<?>) :: Parser a -> String -> Parser a
(FlatParse.Parser f) <?> msg = FlatParse.Parser \r eob s n -> case f r eob s n of
  Fail# -> Err# (ParseError (addr2Pos# eob s) msg)
  x -> x
{-# inline (<?>) #-}

err :: String -> Parser a
err msg = FlatParse.Parser \r eob s n -> Err# (ParseError (addr2Pos# eob s) msg)
{-# inline err #-}
