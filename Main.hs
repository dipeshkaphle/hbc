module Main where
import Parser
import System.Environment
main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
