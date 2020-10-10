module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Expr
import Control.Monad

data LogicalOp = And | Or | Implies | DoubleImplies deriving (Show)

data CmpOp = Lt | Lte | Gt | Gte | Eq | NEq deriving Show



data Expression = IntNode Integer
                | DoubleNode Double
                | BoolNode Bool
                | Not Expression
                | LogicalBinOp LogicalOp Expression Expression
                | CmpNode CmpOp Expression Expression
                | Id String
                | List [Expression]
                | Assign Expression Expression -- the first Expression can only be of type Id String)
                | Tuple [Expression]
                | Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                | Modulus Expression Expression
                | Negation Expression deriving (Show)


parseExprList :: Parser Expression
parseExprList = liftM List $ sepBy parseTerm (comma lexer) --liftM makes the List Constructor Monadic
{- this is the same as above
parseExprList = do
    arr <- sepBy parseTerm (comma lexer)
    return $ List 
-}
-- same thing as above going on in parseTuple
parseTuple :: Parser Expression
parseTuple = liftM Tuple $ sepBy parseTerm (comma lexer)

data Statement = PrintStatement Expression 
    deriving (Show)



lexer :: TokenParser()
lexer = makeTokenParser (haskellStyle {opStart = oneOf "+-*/%|&~=<>"
                                   , opLetter = oneOf "+-*/%|&~=<>"})


parseExpression :: Parser Expression
parseExpression = buildExpressionParser table parseTerm

-- for getting associativity right
table = [[binary "=" (Assign) AssocRight]
        ,[binary "==" (CmpNode Eq) AssocNone,binary "/=" (CmpNode NEq) AssocNone
        ,binary "<" (CmpNode Lt) AssocNone , binary "<=" (CmpNode Lte) AssocNone
        ,binary ">" (CmpNode Gt) AssocNone , binary ">=" (CmpNode Gte) AssocNone]
        ,[prefix "~" (Not)]
        ,[binary "&&"(LogicalBinOp And) AssocRight, binary "||" (LogicalBinOp Or) AssocRight
         ,binary "->" (LogicalBinOp Implies) AssocLeft , binary "<->" (LogicalBinOp DoubleImplies)  AssocLeft]
        ,[prefix "-" (Negation), prefix "+" (id) ]
        ,[binary "*" (Multiplication) AssocLeft, binary "/" (Division) AssocLeft,
         binary "%" (Modulus) AssocLeft]
        ,[binary "+" (Addition) AssocLeft, binary "-" (Subtraction) AssocLeft]]

binary name fun assoc = Infix (do{ reservedOp lexer name; return fun }) assoc
prefix  name fun      = Prefix (do{ reservedOp lexer name; return fun })
postfix name fun      = Postfix (do{ reservedOp lexer name; return fun })


parseNumber :: Parser Expression
parseNumber = do
    num <- naturalOrFloat lexer
    case num of
        Left n -> return $ IntNode n
        Right n -> return $ DoubleNode n

-- to parse the strings passed
parseString :: Monad m => String -> m Expression
parseString str = do
    return $ case str of
        "True" -> BoolNode True
        "False" -> BoolNode False
        otherwise -> Id str


parseTerm :: Parser Expression
parseTerm = parens lexer parseExpression
            <|> parseNumber 
            <|> (identifier lexer >>= parseString )




parsePrint :: Parser Statement
parsePrint = do
    reserved lexer "print"
    expr <- parseExpression
    return $ PrintStatement expr


parseInput :: Parser Expression
parseInput = do
    whiteSpace lexer
    s <- (try $ parseExpression)
        <|> ( try $ parens lexer parseTuple)
        <|> brackets lexer parseExprList
    eof
    return s

calculate str = 
    let ret = parse parseInput "Wrong expr" str in
        case ret of
            Left e -> show e
            Right val -> show val


readExpr = interact (unlines . (map calculate) . lines )
