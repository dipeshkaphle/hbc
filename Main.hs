module Main where

import Control.Monad.State
import Evaluator
import Parser
import System.Environment

main :: IO ()
main = do
  evalStateT evaluator varSymbolTable
