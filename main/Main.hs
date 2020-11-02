-- | Program entry point lies here

module Main where

import Parser
import Value

import Type

main :: IO ()
main = typeCheckMain
