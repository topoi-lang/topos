-- | Parse the topoi source code using this

{-# LANGUAGE OverloadedStrings #-}

module Parsing.Rules where

import Parsing.Parser (Parser)
import qualified Parsing.Parser as Parser
import qualified Tokenising.Tokens as Lexer

import qualified Tokenising.Span as Span
import qualified Name
import qualified Module

someFunc :: String
someFunc = "May the codes passing through this program somehow help to this troubled world."

-- testParse = Parser.parseTokens module' $ Lexer.lexBS "aaa"

-- module' :: Parser (Maybe (Span.Absolute, Name.Module), Module.Header)
-- module' = (,) <$> moduleHeader <*> many definition

moduleHeader :: Parser (Maybe (Span.Absolute, Name.Module), Module.Header)
moduleHeader = undefined
