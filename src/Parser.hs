{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Text.Parsec.String (Parser)
import PrettyParseError
import Text.Parsec

import qualified Data.ByteString.UTF8 as BSU

import Term
import Name

tries :: [Parser a] -> Parser a
tries = choice . map try

parens :: Parser a -> Parser a
parens p = char '(' *> spaces *> p <* char ')' <* spaces

braced :: Parser a -> Parser a
braced p = string "{" *> spaces *> p <* string "}" <* spaces

identifier :: Parser Name
identifier = parsecMap strToName $ many1 letter <* spaces

strToName :: String -> Name
strToName = Name . toName . BSU.fromString

binding :: Parser Name
binding = identifier <|> parsecMap strToName (string "_")

numberP :: Parser Int
numberP = parsecMap read $ many1 digit <* spaces

stringP :: Parser String
stringP = char '\"' *> many alphaNum <* char '\"' <* spaces

atomicLit :: Parser Lit
atomicLit = tries [ LInt <$> numberP
                  , LBool True <$ string "true"
                  , LBool False <$ string "false"
                  , LString <$> stringP
                  ]

atomicTerm :: Parser Term
atomicTerm = (Var <$> identifier)
         <|> (Lit <$> atomicLit)
         <|> parens atomicTerm

typeParse :: Parser Type
typeParse = tries
  [ TInt <$ string "Int"
  , TBool <$ string "Bool"
  ]

bindingWithTypeAnnotation :: Parser (Name, Type)
bindingWithTypeAnnotation = parens ((,) <$> binding <*> (char ':' *> typeParse))

lambda :: Parser Term
lambda = do
  char '\\'
  bindingsWType <- many1 bindingWithTypeAnnotation
  char '.' <* spaces
  term <- atomicTerm
  pure $ foldr (\(name, ty) term -> Lam name ty term) term bindingsWType

-- entrypoint
termParser :: Parser Term
termParser = spaces *> lambda <* eof

-- And an example input for it:
example :: String
example = unlines [ "\\(i:Int) (b:Bool) . \"123\""]

main' :: IO ()
main' = do
  let filename = "example.topoi"
  case parse termParser filename example of
    Left e -> do
      putStrLn $ "\n\ESC[7m " <> filename <> " \ESC[0m\n"
      putStrLn (prettyParseError prettyParseErrorDefaults e example)

    Right tree -> do
      print tree