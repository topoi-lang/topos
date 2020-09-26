-- | Contains all the token and regex stuff

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Parsing.Tokens where

import Text.RawString.QQ

data Tok = TokEOF
         | TokLet
         | TokIn
         | TokWhere
         | TokLowerIdent
         | TokUpperIdent
         | TokNatNum
         | TokFloatNum
         deriving (Eq, Ord)

instance Show Tok where
  show = \case
    TokEOF -> ""
    TokLet -> "let"
    TokIn -> "in"
    TokWhere -> "where"

blockCommentRE :: String
blockCommentRE = [r|{-(.|\n)*-}|]

lineCommentRE :: String
lineCommentRE = [r|--(.*)|]

kwLetRE, kwInRE, kwWhereRE :: String
(kwLetRE, kwInRE, kwWhereRE) = ("let", "in", "where")

lowerIdentRE :: String
lowerIdentRE = [r|[[:lower:]]([[:alnum:]]|_|')*|]

upperIdentRE :: String
upperIdentRE = [r|[[:upper:]]([[:alnum:]]|_|')*|]

naturalNumRE :: String
naturalNumRE = [r|[0-9]+|]

floatingNumRE :: String
floatingNumRE = [r|([0-9]*[.])?[0-9]+|]
