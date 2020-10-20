module Evaluator where

import           Parser
import           Text.ParserCombinators.Parsec hiding (spaces)

evalStatement :: Statement -> IO ()
evalStatement (PrintStatement expr) = do
    val <- eval expr
    putStrLn $ show val

evalStatement (Eval expr) = do
    eval expr
    return ()

eval (IntNode n) = return n

main = do
    inp <- getLine
    evalStatement $ parseInput inp
