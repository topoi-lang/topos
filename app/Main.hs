module Main (main) where

import System.Environment (getArgs)
import Z.Data.Vector (packASCII)

import qualified Parser

main :: IO ()
main = do
    args <- getArgs
    src <- readFile (head $ tail args)
    print . show $ Parser.parse (packASCII src)
