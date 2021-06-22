module Main (main) where

import Prelude hiding (readFile)

import Z.IO (getArgs)
import Z.IO.FileSystem.Base (readFile)

import qualified Parser

main :: IO ()
main = do
    args <- getArgs
    src <- readFile (head $ tail args)
    print $ Parser.parse src
