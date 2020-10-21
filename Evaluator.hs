module Evaluator where

import           Operations
import           Parser
import           Text.ParserCombinators.Parsec hiding (spaces)


data EvalResult = EvalResult {
                             int     :: Maybe Integer
                             ,double :: Maybe Double
                             ,bool   :: Maybe Bool
                             } deriving (Show)

makeEvalResult :: (Maybe Integer, Maybe Double, Maybe Bool) -> EvalResult
makeEvalResult (x,y,z) = EvalResult {int = x, double=y, bool = z}



-- evaluation of different classes of expressions
eval :: Expression -> EvalResult
eval (IntNode n)    = makeEvalResult (Just n, Nothing, Nothing)
eval (BoolNode b)   = makeEvalResult (Nothing,Nothing,Just b)
eval (DoubleNode n) = makeEvalResult (Nothing,Just n, Nothing)
eval (Not expr)     = makeEvalResult $ (Nothing, Nothing, fmap not (bool (eval expr)))
-- Make comparision between numbers
-- the numbers can be int or double
eval (CmpNode op expr1 expr2) =
    let val1 = eval expr1
        val2 = eval expr2
        operation = getCmpOperator op
        convertToDouble = fmap fromIntegral
        p1   = operation <$> (convertToDouble (int val1)) <*> (convertToDouble (int val2))
        p2   = operation <$> (convertToDouble (int val1)) <*> (double val2)
        p3   = operation <$> (double val1) <*> (convertToDouble (int val2))
        p4   = operation <$> (double val1) <*> (double val2)
        cmpResult = (filter (\x -> x /= Nothing) [p1,p2,p3,p4])
        res
          | ((length cmpResult) == 0) = Nothing
          | otherwise = (cmpResult !! 0)
     in makeEvalResult (Nothing, Nothing, res )



evalStatement :: Statement -> IO()
-- we're evaluating a print statement
-- dealing with multiple data types is complicated in haskell
-- out eval function returns a Maybe for all three types we use packed into
-- a record names EvalResult
-- we just pattern match it and print
evalStatement (PrintStatement expr)  =  do
    let ans = eval expr
        printVal val = putStrLn $ show val
    case ans of
        EvalResult {int= Just n}    -> printVal n
        EvalResult {double= Just n} -> printVal n
        EvalResult {bool = Just b}  -> printVal b
        otherwise -> error $ "Invalid expression" ++ show expr

-- seq is used here because we want the
-- expr to be evaluated for sure
-- if we do just
-- `do
-- eval expr
-- return ()`
-- we wont have our eval expr ever evaluated because of
-- haskell's laziness as it didnt really need it
-- but since our expression has to be evaluated in any case
-- so we use seq to  do it
-- https://wiki.haskell.org/Seq
-- seq :: a->b->b
evalStatement (Eval expr) = do
    eval expr `seq` return ()


main = do
    inp <- getLine
    evalStatement $ parseInput inp
