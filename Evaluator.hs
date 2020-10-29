module Evaluator where

import           Control.Monad.State
import qualified Data.Map                      as M
import           Operations
import           Parser
import           System.Environment
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

type SymbolTableVal = Either Expression FunctionBody

type Result a = StateT (M.Map String SymbolTableVal) IO a

data EvalResult = EvalResult {
                             int     :: Maybe Integer
                             ,double :: Maybe Double
                             ,bool   :: Maybe Bool
                             } deriving (Show)

makeEvalResult :: (Maybe Integer, Maybe Double, Maybe Bool) -> EvalResult
makeEvalResult (x,y,z) = EvalResult {int = x, double=y, bool = z}


varSymbolTable :: M.Map String SymbolTableVal
varSymbolTable = M.fromList[
                           ("pi",Left $ DoubleNode 3.14),
                           ("e", Left $ DoubleNode (exp 1))]

-- evaluation of different classes of expressions
eval :: Expression -> Result EvalResult
eval (IntNode n)    = return $ makeEvalResult (Just n, Nothing, Nothing)
eval (BoolNode b)   = return $ makeEvalResult (Nothing,Nothing,Just b)
eval (DoubleNode n) = return $  makeEvalResult (Nothing,Just n, Nothing)
eval (Not expr)     = do
    evaluatedExpr <- eval expr
    return $ makeEvalResult $ (Nothing, Nothing, fmap not (bool (evaluatedExpr)))

eval (Id str) = do
    varTable <- get
    case M.lookup str varTable of
        Nothing -> error ("Variable "++str ++ " isnt defined")
        Just a  -> case a of
            (Right func) -> error "Call function with format foo(a,b)"
            (Left val)   -> eval val


eval (Assign str expr) = do
    ans      <- eval expr
    let getStringFromId (Id name) = name
        mod = \x -> modify $ (M.insert (getStringFromId str) x)
    case ans of
        EvalResult {int= Just n}    -> mod $ Left $ IntNode n
        EvalResult {double= Just n} -> mod $ Left $ DoubleNode n
        EvalResult {bool = Just b}  -> mod $ Left $ BoolNode b
    return ans

eval (LogicalBinOp op expr1 expr2) = do
    val1 <- eval expr1
    val2 <- eval expr2
    let operationToDo = getLogicalOperator op
        f p q = operationToDo p q
        g = makeEvalResult
        x =  case (val1,val2) of
         (EvalResult {bool = Just p}, EvalResult {bool = Just q}) -> Just $ f p q
         otherwise                                                -> Nothing
     in return $ g (Nothing, Nothing, x)

-- Make comparision between numbers
-- the numbers can be int or double
eval (CmpNode op expr1 expr2) = do
    val1 <- eval expr1
    val2 <- eval expr2
    let operation = getCmpOperator op
        convertToDouble = fmap fromIntegral
        p1   = operation <$> (convertToDouble (int val1)) <*> (convertToDouble (int val2))
        p2   = operation <$> (convertToDouble (int val1)) <*> (double val2)
        p3   = operation <$> (double val1) <*> (convertToDouble (int val2))
        p4   = operation <$> (double val1) <*> (double val2)
        cmpResult = (filter (\x -> x /= Nothing) [p1,p2,p3,p4])
        res
          | ((length cmpResult) == 0) = Nothing
          | otherwise = (cmpResult !! 0)
     in return $ makeEvalResult (Nothing, Nothing, res )

eval (Addition expr1 expr2)  = commonEval (+) expr1 expr2
eval (Subtraction expr1 expr2) = eval (Addition expr1 (Negation expr2))
eval (Negation expr1) = do
    val1 <- eval expr1
    let g = makeEvalResult
        f a = fmap (negate) (Just a)
     in return $ case val1 of
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
eval (Division expr1 expr2) = do
    val1 <- eval expr1
    val2 <- eval expr2
    let g = makeEvalResult
        f = fromIntegral
        h = divideWithZeroDivError
        x =  case (val1,val2) of
                 (EvalResult {int = Just m},EvalResult {int = Just n})       -> Just (h (f m) (f n))
                 (EvalResult {int = Just m},EvalResult {double = Just n})    -> Just (h (f m) n)
                 (EvalResult {double = Just m},EvalResult {int = Just n})    -> Just (h m (f n))
                 (EvalResult {double = Just m},EvalResult {double = Just n}) -> Just (h m n)
                 otherwise                                                   -> Nothing
    return $ g (Nothing,x,Nothing)

-- this function is used so that i dont have to repeat code
-- for multiplication and Addition and Power, i can just pass  the func to be
-- + , *, **  and modulus and somewhat in divide as well and it'll evaluate properly
commonEval func expr1 expr2 = do
    val1 <- eval expr1
    val2 <- eval expr2
    let f a b = fmap (func) (Just a) <*> (Just b)
        g = makeEvalResult
        ans = case (val1,val2) of
         (EvalResult {int = Just m},EvalResult {int = Just n})       -> g (fmap truncate
             (f (fromIntegral m) (fromIntegral n)),Nothing,Nothing)
         (EvalResult {int= Just m},EvalResult {double= Just n})      -> g (Nothing,f (fromIntegral m) n, Nothing)
         (EvalResult {double = Just m},EvalResult {int = Just n})    -> g (Nothing,f m (fromIntegral n), Nothing)
         (EvalResult {double = Just m},EvalResult {double = Just n}) -> g (Nothing , f m n, Nothing)
         otherwise                                                   -> g (Nothing, Nothing, Nothing)
    return ans



evalStatement :: Statement -> Result ()
-- we're evaluating a print statement
-- dealing with multiple data types is complicated in haskell
-- out eval function returns a Maybe for all three types we use packed into
-- a record names EvalResult
-- we just pattern match it and print
evalStatement (PrintStatement expr)  =  do
    ans <- eval expr
    let printVal val = liftIO $ putStrLn $ show val
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

-- this had to be made so that we could propagate the errors
-- in the evalStatement for Eval expr , if i had done just (eval expr) `seq` (return ())
-- the error wouldnt propagate that way. I  guess it was like that because we hadnt really
-- done anything with those errors.
-- So i had to do this so that not only there was just the evaluation of the expr
-- but also the errors would get evaluated when i convert them to string with show
-- Now with this, errors get propagated when we `seq` it with return ()
evalStatement (Eval expr) = do
     a <- eval expr
     let f a = (show a) `seq` return ()
     case a of
        EvalResult {int = Just m}     -> f m
        EvalResult {double = Just m } -> f m
        EvalResult {bool = Just m}    -> f m
        otherwise                     -> error $ "Invalid expression " ++ show expr


-- takes in an input string and parses it and runs evalStatement on it
evaluate :: String -> Result ()
evaluate s = let parsedInput = parseInput s
               in evalStatement parsedInput

-- reads input with getContents and evaluates line by line
evaluator :: Result ()
-- evaluator = liftIO getContents >>= (mapM_ evaluate) . lines
-- equivalent code but doesnt use getContents which kinda seems bad
evaluator = do
    liftIO $ putStr ">>>>"
    liftIO $ hFlush stdout
    a <- liftIO $ getLine
    case a of
        "" -> do
            liftIO $ putStrLn "Done"
        _  -> do
            evaluate a
            evaluator


-- runs evaluator with our state (i.e symbolTable)
--main = do
 --   evalStateT (evaluator) varSymbolTable
