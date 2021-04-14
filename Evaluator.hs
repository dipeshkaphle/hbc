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

data EvalResult = EvalResult
  { int    :: Maybe Integer,
    double :: Maybe Double,
    bool   :: Maybe Bool
  }
  deriving (Show)

makeEvalResult :: (Maybe Integer, Maybe Double, Maybe Bool) -> EvalResult
makeEvalResult (x, y, z) = EvalResult {int = x, double = y, bool = z}

varSymbolTable :: M.Map String SymbolTableVal
varSymbolTable =
  M.fromList
    [ ("pi", Left $ DoubleNode 3.14),
      ("e", Left $ DoubleNode (exp 1))
    ]

-- evaluation of different classes of expressions
eval :: Expression -> Result EvalResult
eval (IntNode n) = return $ makeEvalResult (Just n, Nothing, Nothing)
eval (BoolNode b) = return $ makeEvalResult (Nothing, Nothing, Just b)
eval (DoubleNode n) = return $ makeEvalResult (Nothing, Just n, Nothing)
eval (Not expr) = do
  evaluatedExpr <- eval expr
  return $ makeEvalResult (Nothing, Nothing, fmap not (bool evaluatedExpr))
eval (Id str) = do
  varTable <- get
  case M.lookup str varTable of
    Nothing -> error ("Variable " ++ str ++ " isnt defined")
    Just a -> case a of
      (Right func) -> error "Call function with format foo(a,b)"
      (Left val)   -> eval val
eval (Assign (Id str) expr) = do
  ans <- eval expr
  let getStringFromId (Id name) = name
      mod = modify . M.insert str
  case ans of
    EvalResult {int = Just n}    -> mod $ Left $ IntNode n
    EvalResult {double = Just n} -> mod $ Left $ DoubleNode n
    EvalResult {bool = Just b}   -> mod $ Left $ BoolNode b
  return ans
eval (Assign lval rval) =
  return (error $ "Invalid lvalue: " ++ show lval)
eval (LogicalBinOp op expr1 expr2) = do
  val1 <- eval expr1
  val2 <- eval expr2
  let operationToDo = getLogicalOperator op
      f p q = operationToDo p q
      g = makeEvalResult
      x = case (val1, val2) of
        (EvalResult {bool = Just p}, EvalResult {bool = Just q}) -> Just $ f p q
        _                                                        -> Nothing
   in return $ g (Nothing, Nothing, x)

-- Make comparision between numbers
-- the numbers can be int or double
eval (CmpNode op expr1 expr2) = do
  val1 <- eval expr1
  val2 <- eval expr2
  let operation = getCmpOperator op
      convertToDouble = fmap fromIntegral
      p1 = operation <$> convertToDouble (int val1) <*> convertToDouble (int val2)
      p2 = operation <$> convertToDouble (int val1) <*> double val2
      p3 = operation <$> double val1 <*> convertToDouble (int val2)
      p4 = operation <$> double val1 <*> double val2
      cmpResult = filter (/= Nothing) [p1, p2, p3, p4]
      res
        | null cmpResult = Nothing
        | otherwise = head cmpResult
   in return $ makeEvalResult (Nothing, Nothing, res)
eval (Addition expr1 expr2) = commonEval (+) expr1 expr2
eval (Subtraction expr1 expr2) = eval (Addition expr1 (Negation expr2))
eval (Negation expr1) = do
  val1 <- eval expr1
  let g = makeEvalResult
      f a = fmap negate (Just a)
   in return $ case val1 of
        EvalResult {int = Just m}    -> g (f m, Nothing, Nothing)
        EvalResult {double = Just m} -> g (Nothing, f m, Nothing)
        _                            -> g (Nothing, Nothing, Nothing)
eval (Multiplication expr1 expr2) = commonEval (*) expr1 expr2
eval (Power expr1 expr2) = commonEval (**) expr1 expr2
eval (Modulus expr1 expr2) = commonEval modulus expr1 expr2
-- I am always keeeping the result of divide  as double
-- So I cant quite use the commonEval function
-- also used divideWithZeroDivError(defined in Operations.hs)
-- so that we can throw errors when we divide by zero
eval (Division expr1 expr2) = do
  val1 <- eval expr1
  val2 <- eval expr2
  let g = makeEvalResult
      f = fromIntegral
      h = divideWithZeroDivError -- defined in Operations.hs
      x = case (val1, val2) of
        (EvalResult {int = Just m}, EvalResult {int = Just n}) -> Just (h (f m) (f n))
        (EvalResult {int = Just m}, EvalResult {double = Just n}) -> Just (h (f m) n)
        (EvalResult {double = Just m}, EvalResult {int = Just n}) -> Just (h m (f n))
        (EvalResult {double = Just m}, EvalResult {double = Just n}) -> Just (h m n)
        _ -> Nothing
  return $ g (Nothing, x, Nothing)
eval (Invoke funcName funcArgs) = evalFunctions (Invoke funcName funcArgs)

-- this function is used so that i dont have to repeat code
-- for multiplication and Addition and Power, i can just pass  the func to be
-- + , *, **  and modulus and somewhat in divide as well and it'll evaluate properly
commonEval func expr1 expr2 = do
  val1 <- eval expr1
  val2 <- eval expr2
  let f a b = fmap func (Just a) <*> Just b
      g = makeEvalResult
      ans = case (val1, val2) of
        (EvalResult {int = Just m}, EvalResult {int = Just n}) ->
          g
            ( fmap
                truncate
                (f (fromIntegral m) (fromIntegral n)),
              Nothing,
              Nothing
            )
        (EvalResult {int = Just m}, EvalResult {double = Just n}) -> g (Nothing, f (fromIntegral m) n, Nothing)
        (EvalResult {double = Just m}, EvalResult {int = Just n}) -> g (Nothing, f m (fromIntegral n), Nothing)
        (EvalResult {double = Just m}, EvalResult {double = Just n}) -> g (Nothing, f m n, Nothing)
        _ -> g (Nothing, Nothing, Nothing)
  return ans

------------------------------------------------------------------------
-- Evaluating some built in functions
-- It kinda has to be done this way
-- for user defined functions we'll be adopting a separate method
-- built in functions will be trigonometric functions, logarithms,
-- factorial, hyperbolic trigonometric functions and maybe some more
------------------------------------------------------------------------

handleWrongNumberOfArgsError funcName noOfArgsDesired noOfArgsPassed =
  error $ "Wrong number of arguments passed to " ++ funcName ++ ". Expected: " ++ show n ++ " Got: " ++ show n'
  where
    n = noOfArgsDesired
    n' = noOfArgsPassed

oneArgumentFunctionEval function functionName (Tuple arguments) =
  case length arguments of
    1 -> do
      val <- eval $ head arguments
      let g = makeEvalResult
          f = fromIntegral
          x = case val of
            EvalResult {int = Just m}    -> function <$> Just (f m)
            EvalResult {double = Just m} -> function <$> Just m
            _                            -> Nothing
      return $ g (Nothing, x, Nothing)
    _ -> handleWrongNumberOfArgsError functionName 1 (length arguments)

evalFunctions :: Expression -> Result EvalResult
evalFunctions (Invoke "sin" arguments) = oneArgumentFunctionEval sin "sin" arguments
evalFunctions (Invoke "cos" arguments) = oneArgumentFunctionEval cos "cos" arguments
evalFunctions (Invoke "tan" arguments) = oneArgumentFunctionEval tan "tan" arguments
evalFunctions (Invoke "asin" arguments) = oneArgumentFunctionEval asin "asin" arguments
evalFunctions (Invoke "acos" arguments) = oneArgumentFunctionEval acos "acos" arguments
evalFunctions (Invoke "atan" arguments) = oneArgumentFunctionEval atan "atan" arguments
evalFunctions (Invoke "sinh" arguments) = oneArgumentFunctionEval sinh "sinh" arguments
evalFunctions (Invoke "cosh" arguments) = oneArgumentFunctionEval cosh "cosh" arguments
evalFunctions (Invoke "tanh" arguments) = oneArgumentFunctionEval tanh "tanh" arguments
evalFunctions (Invoke "asinh" arguments) = oneArgumentFunctionEval asinh "asinh" arguments
evalFunctions (Invoke "acosh" arguments) = oneArgumentFunctionEval acosh "acosh" arguments
evalFunctions (Invoke "atanh" arguments) = oneArgumentFunctionEval atanh "atanh" arguments
evalFunctions (Invoke "log" arguments) = oneArgumentFunctionEval log "log" arguments
evalFunctions (Invoke "logBase" (Tuple arguments)) =
  case length arguments of
    2 -> do
      base <- eval $ head arguments
      let f g b = oneArgumentFunctionEval g (show $ "logBase " ++ show b) (Tuple (tail arguments))
      case base of
        EvalResult {int = Just m} -> f (logBase (fromIntegral m)) m
        EvalResult {double = Just m} -> f (logBase m) m
        _ -> return $ makeEvalResult (Nothing, Nothing, Nothing)
    _ -> handleWrongNumberOfArgsError "logBase" 2 (length arguments)
evalFunctions (Invoke "sqrt" arguments) = oneArgumentFunctionEval sqrt "sqrt" arguments
-- cant use the oneArgumentFunctionEval because it will change the result to double always
-- but we want to maintain its type in case it is an integer
evalFunctions (Invoke "abs" (Tuple arguments)) =
  case length arguments of
    1 -> do
      val <- eval $ head arguments
      let g = makeEvalResult
      return $ g (abs <$> int val, abs <$> double val, Nothing)
    _ -> handleWrongNumberOfArgsError "abs" 1 (length arguments)
evalFunctions (Invoke "factorial" (Tuple arguments)) =
  case length arguments of
    1 -> do
      val <- eval $ head arguments
      let g = makeEvalResult
          fact acc n =
            if n <= 0
              then case n of
                0 -> acc
                _ -> -1
              else fact (acc * n) (n -1)
      return $ case val of
        EvalResult {int = Just m} -> g (Just (fact 1 m), Nothing, Nothing)
        _                         -> g (Nothing, Nothing, Nothing)
    _ -> handleWrongNumberOfArgsError "factorial" 1 (length arguments)
evalFunctions (Invoke functionName (Tuple arguments)) = do
  varTable <- get
  case M.lookup functionName varTable of
    Nothing -> error ("Function " ++ functionName ++ " isnt defined")
    Just a -> case a of
      (Right (FunctionBody name argsName statementsToExecute)) -> case argsName of
        Tuple argsNameList ->
          if length argsNameList == length arguments
            then
              evalUserDefinedFunction
                functionName
                (map get_str_out_of_id argsNameList)
                statementsToExecute
                (M.fromList (zip (map get_str_out_of_id argsNameList) (map Left arguments)))
            else
              error $
                "Expected " ++ show (length argsNameList) ++ " arguments for function " ++ functionName
                  ++ ". But got "
                  ++ show (length arguments)
                  ++ " arguments"
        _ -> error "You shouldnt have ever reached this error, This program is doomed if you are seeing this."
      (Left val) -> error "Call function with format foo(a,b)"

foo :: Expression -> StateT (M.Map String SymbolTableVal) IO (Either Expression FunctionBody)
foo x = do
  ans <- eval x
  return $ case ans of
    EvalResult {int = Just n} -> Left $ IntNode n
    _                         -> Left $ IntNode 0

-- helper func
get_str_out_of_id :: Expression -> String
get_str_out_of_id x = case x of
  Id name -> name
  _       -> ""

-- this doesnt work at all
-- I gotta make it work idk how
--
--

conflict_resolve varTable k v1 v2 = case v1 of
  Left tok -> case tok of
    Id x -> case M.lookup x varTable of
      Nothing -> error $ x ++ " passed as parameter is invalid"
      Just y -> case y of
        Left val -> y
        _        -> error "Functions arent first class citizens yet"
    _ -> v1
  Right tok -> v1

evalUserDefinedFunction :: String -> [String] -> [Statement] -> M.Map String SymbolTableVal -> Result EvalResult
evalUserDefinedFunction functionName argsNameList statementsToExecute stackSymTable = do
  varTable <- get
  modify $ M.unionWithKey (conflict_resolve varTable) stackSymTable
  varTable2 <- get
  execStateT (lift $ evalFunctionStaements statementsToExecute) varTable2
  modify $ flip (M.differenceWithKey (\k v1 v2 -> M.lookup k varTable)) stackSymTable
  return $ makeEvalResult (Just 0, Nothing, Nothing)

evalFunctionStaements :: [Statement] -> Result ()
evalFunctionStaements x = do
  varTable <- get
  mapM_ evalStatement x

------------------------------------------------------------------------
------------------------------------------------------------------------

evalStatement :: Statement -> Result ()
-- we're evaluating a print statement
-- dealing with multiple data types is complicated in haskell
-- out eval function returns a Maybe for all three types we use packed into
-- a record names EvalResult
-- we just pattern match it and print
evalStatement (PrintStatement expr) = do
  ans <- eval expr
  let printVal val = liftIO $ print val
  case ans of
    EvalResult {int = Just n}    -> printVal n
    EvalResult {double = Just n} -> printVal n
    EvalResult {bool = Just b}   -> printVal b
    _                            -> error $ "Invalid expression: " ++ show expr

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
  let f a = show a `seq` return ()
  case a of
    EvalResult {int = Just m}    -> f m
    EvalResult {double = Just m} -> f m
    EvalResult {bool = Just m}   -> f m
    _                            -> error $ "Invalid expression " ++ show expr
evalStatement (Define (FunctionBody funcName functionArgs statements)) =
  modify $ M.insert funcName (Right $ FunctionBody funcName functionArgs statements)

-- takes in an input string and parses it and runs evalStatement on it
evaluate :: String -> Result ()
evaluate s =
  let parsedInput = parseInput s
   in evalStatement parsedInput

-- reads input with getContents and evaluates line by line
evaluator :: Result ()
-- evaluator = liftIO getContents >>= (mapM_ evaluate) . lines
-- equivalent code but doesnt use getContents which kinda seems bad
evaluator = do
  liftIO $ putStr ">>>>"
  liftIO $ hFlush stdout
  a <- liftIO getLine
  case a of
    "" ->
      liftIO $ putStrLn "Done"
    _ -> do
      evaluate a
      evaluator

-- runs evaluator with our state (i.e symbolTable)
--main = do
--   evalStateT (evaluator) varSymbolTable
