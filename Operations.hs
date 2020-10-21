module Operations where

import           Parser

getCmpOperator :: Ord a => CmpOp -> (a -> a -> Bool)
getCmpOperator op = case op of
    Lte    -> (<=)
    Lt     -> (<)
    Gt     -> (>)
    Gte    -> (>=)
    Equal  -> (==)
    NEqual -> (/=)

getLogicalOperator :: LogicalOp -> (Bool -> Bool -> Bool)
getLogicalOperator op = case op of
    And           -> (&&)
    Or            -> (||)
    Xor           -> (xor)
    Implies       -> (implies)
    DoubleImplies -> (doubleImplies)





-- some logical operators that are not defined in haskell by default
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _     _    = False

implies :: Bool -> Bool -> Bool
implies p q = (not p) || q

doubleImplies :: Bool -> Bool -> Bool
doubleImplies p q = (p `implies` q) && (q `implies` q)
