-- | Program entry point lies here

module Main where

import Parser
import Value

main :: IO ()
main = mainEval
