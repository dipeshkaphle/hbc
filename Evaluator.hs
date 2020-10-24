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

eval (Addition expr1 expr2)  = commonEval (+) expr1 expr2
eval (Subtraction expr1 expr2) = eval (Addition expr1 (Negation expr2))
eval (Negation expr1) =
    let val1 = eval expr1
        g = makeEvalResult
        f a = fmap (negate) (Just a)
     in case val1 of
         EvalResult {int = Just m}    -> g (f m, Nothing, Nothing)
         EvalResult {double = Just m} -> g (Nothing, f m, Nothing)
         otherwise                    -> g (Nothing, Nothing, Nothing)


eval (Multiplication expr1 expr2) = commonEval (*) expr1 expr2
eval (Power expr1 expr2) = commonEval (**) expr1 expr2
eval (Modulus expr1 expr2) = commonEval (modulus) expr1 expr2

-- I am always keeeping the result of divide  as double
-- So I cant quite use the commonEval function
-- also used divideWithZeroDivError(defined in Operations.hs)
-- so that we can throw errors when we divide by zero
eval (Division expr1 expr2) =
    let val1 = eval expr1
        val2 = eval expr2
        g = makeEvalResult
        f = fromIntegral
        h = divideWithZeroDivError
        x =  case (val1,val2) of
                 (EvalResult {int = Just m},EvalResult {int = Just n})       -> Just (h (f m) (f n))
                 (EvalResult {int = Just m},EvalResult {double = Just n})    -> Just (h (f m) n)
                 (EvalResult {double = Just m},EvalResult {int = Just n})    -> Just (h m (f n))
                 (EvalResult {double = Just m},EvalResult {double = Just n}) -> Just (h m n)
                 otherwise                                                   -> Nothing
    in g (Nothing,x,Nothing)

-- this function is used so that i dont have to repeat code
-- for multiplication and Addition and Power, i can just pass  the func to be
-- + , *, **  and modulus and somewhat in divide as well and it'll evaluate properly
commonEval func expr1 expr2 =
    let val1 = eval expr1
        val2 = eval expr2
        f a b = fmap (func) (Just a) <*> (Just b)
        g = makeEvalResult
     in case (val1,val2) of
         (EvalResult {int = Just m},EvalResult {int = Just n})       -> g (fmap truncate
             (f (fromIntegral m) (fromIntegral n)),Nothing,Nothing)
         (EvalResult {int= Just m},EvalResult {double= Just n})      -> g (Nothing,f (fromIntegral m) n, Nothing)
         (EvalResult {double = Just m},EvalResult {int = Just n})    -> g (Nothing,f m (fromIntegral n), Nothing)
         (EvalResult {double = Just m},EvalResult {double = Just n}) -> g (Nothing , f m n, Nothing)
         otherwise                                                   -> g (Nothing, Nothing, Nothing)

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
-- we're using evalAndDoNothing so that we can propagate errors
-- Had we done something like (eval expr) `seq` (return ()), the  errors wouldnt
-- get propagated and this would run successfully
-- So we not only had to force the evaluation of the expression but also
-- the evaluation of the errors that mightve popped in during the evaluation of
-- the expression and hence we use evalAndDoNothing as it forces evaluation of the errors
-- as well by trying to convert them to strings with show(which throws error).
evalStatement (Eval expr) = (evalAndDoNothing expr) `seq` (return ())

-- this had to be made so that we could propagate the errors
-- in the evalStatement for Eval expr , if i had done just (eval expr) `seq` (return ())
-- the error wouldnt propagate that way. I  guess it was like that because we hadnt really
-- done anything with those errors.
-- So i had to do this so that not only there was just the evaluation of the expr
-- but also the errors would get evaluated when i convert them to string with show
-- Now with this, errors get propagated when we `seq` it with return ()
evalAndDoNothing  expr =
    let a = eval expr
     in case a of
        EvalResult {int = Just m}     -> show m
        EvalResult {double = Just m } -> show m
        EvalResult {bool = Just m}    -> show m
        otherwise                     -> error $ "Invalid expression " ++ show expr

main = do
    inp <- getLine
    evalStatement $ parseInput inp
