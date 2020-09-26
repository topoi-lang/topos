-- | Contains all the token and regex stuff

{-# LANGUAGE QuasiQuotes #-}

module Parsing.Tokens where

import Text.RawString.QQ

data Tok = TokEOF
         | TokComment -- [TODO] Will keep track the offset later.
         | TokLet
         | TokIn
         | TokWhere
         deriving (Eq, Ord)

instance Show Tok where
  show tok = case tok of
    TokEOF -> ""
    TokComment -> "[redacted]" -- [TODO] Will keep track the comment later.
    TokLet -> "let"
    TokIn -> "in"
    TokWhere -> "where"

blockCommentRE :: String
blockCommentRE = [r|{-(.|\n)*-}|]

lineCommentRE :: String
lineCommentRE = [r|--(.*)|]

kwLetRE, kwInRE, kwWhereRE :: String
(kwLetRE, kwInRE, kwWhereRE) = ("let", "in", "where")
