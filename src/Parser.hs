
-- | This module is in charge of parsing expressions.
module Parser(getExpression, getProgram) where

import Prelude hiding (catch)
import Text.Parsec.Prim hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String hiding (Parser)
import Text.Parsec.Char hiding (spaces)
import Text.Parsec.Expr
import LiPL
import Control.Monad (guard)
import Control.Monad.State
import qualified Control.Exception as C
import Data.Functor.Identity (Identity)
import Data.Either (either)

type Parser = Parsec String [String]

-- | Fresh variables
freshVars :: [String]
freshVars = map ((:) 'b' . show) [0..]

getFreshVar :: Parser String
getFreshVar = do
	(x:xs) <- getState
	putState xs
	return x

-- | Keywords that identifiers can not take.
keywords :: [String]
keywords = ["if", "fun", "else", "then", "end", "let", "int", "bool", "in", "true", "false", "recfun", "head", "tail", "empty"]

-- | Parser for an entire program.
program :: Parser Expression
program = do
	skipMany space
	e <- expression
	skipMany space
	eof
	return e

-- | Parser for at least 1 space.
spaces :: Parser ()
spaces = skipMany1 space

-- | Parser expressions.
expression :: Parser Expression
expression = try operation <|> nonoperation

-- | Parser for non-operations.
nonoperation :: Parser Expression
nonoperation = try application <|>
			   try list <|>
			   try empty <|>
			   try headList <|>
			   try tailList <|>
			   try letBinding <|> 
			   try number <|> 
			   try boolean <|> 
			   try ifThenElse <|>
			   try recfun <|>
			   try fun <|> 
			   try identifier <|>
			   inParens expression

-- | Table of operation.
optable = [
			[Prefix (unary "\\")],
			[Infix (binary "*") AssocLeft, Infix (binary "/") AssocLeft],
			[Infix (binary "+") AssocLeft, Infix (binary "-") AssocLeft],
			[Infix (binary "<") AssocLeft, Infix (binary ">") AssocLeft, Infix (binary "=") AssocLeft],
			[Infix (binary "&") AssocLeft, Infix (binary "|") AssocLeft],
			[Infix cons AssocRight]
		]

application :: Parser Expression
application = do
	char '('
	skipMany space
	f <- expression
	skipMany space
	vs <- try $ sepBy1 expression $ try spaces
	skipMany space
	char ')'
	return $ foldl (\x y -> Application x y) f vs

-- | Parser for operations.
operation :: Parser Expression
operation = buildExpressionParser optable (try nonoperation <|> inParens operation)

-- | Returns the same parser in parentheses.
inParens :: Parser a -> Parser a
inParens p = try $ do
	char '('
	skipMany space
	r <- p
	skipMany space
	char ')'
	return r

inBrackets :: Parser a -> Parser a
inBrackets p = try $ do
	char '{'
	skipMany space
	r <- p
	skipMany space
	char '}'
	return r

-- | Binary operation.
binary :: String -> Parser (Expression -> Expression -> Expression)
binary c = try $ do
	optional spaces
	string c
	optional spaces
	return $ Binary c

-- | Unary operation.
unary :: String -> Parser (Expression -> Expression)
unary c = try $ do
	optional spaces
	string c
	optional spaces
	return $ Unary c

-- | Parser for identifier strings.
identifierString :: Parser String
identifierString = do
	h <- letter
	i <- many alphaNum
	guard (not $ h:i `elem` keywords)
	return $ h:i

-- | Parser for the list construction operation.
cons :: Parser (Expression -> Expression -> Expression)
cons = try $ do
	many space
	string "::"
	many space
	return (\h t -> List h t)

-- | Parser of lists in the form [a, b, c].
list :: Parser Expression
list = do
	char '['
	many space
	es <- sepBy expression $ try (many space >> char ',' >> many space)
	many space
	char ']'
	let e = foldr (\h t -> List h t) Nil es
	return e

-- | Parser for Head expressions.
headList :: Parser Expression
headList = do
	char '('
	skipMany space
	string "head"
	spaces
	e <- expression
	skipMany space
	char ')'
	return $ Head e

-- | Parser for Tail expressions.
tailList :: Parser Expression
tailList = do
	char '('
	skipMany space
	string "tail"
	spaces
	e <- expression
	skipMany space
	char ')'
	return $ Tail e

-- | Parser for IsNil expressions.
empty :: Parser Expression
empty = do
	char '('
	skipMany space
	string "empty"
	spaces
	e <- expression
	skipMany space
	char ')'
	return $ (IsNil e)

-- | Parser for identifiers.
identifier :: Parser Expression
identifier = do
	s <- identifierString
	return $ Identifier s

-- | Parser for constant number expressions.
number :: Parser Expression
number = do
	f <- option id (char '-' >> return negate)
	xs <- many1 digit
	return $ Number $ f $ read xs

-- | Parser for constant boolean expressions.
boolean :: Parser Expression
boolean = do
	xs <- string "true" <|> string "false"
	return $ Boolean $ xs == "true"


-- | Parser for IfThenElse expressions.
ifThenElse :: Parser Expression
ifThenElse = do
	string "if"
	skipMany space
	i <- expression
	skipMany space
	string "then"
	skipMany space
	t <- expression
	skipMany space
	string "else"
	skipMany space
	e <- expression
	skipMany space
	string "end"
	return $ IfThenElse i t e

-- | Parser for anonymous recursive function definitions.
recfun :: Parser Expression
recfun = do
	string "recfun"
	spaces
	n <- identifierString
	t <- try (spaces >> inBrackets types) <|> fmap TypeVar getFreshVar
	(v:vs) <- many1 $ try $ spaces >> identifierString
	skipMany space
	string "->"
	skipMany space
	e <- expression
	skipMany space
	string "end"
	ts <- sequence $ replicate (length vs) $ fmap TypeVar getFreshVar
	return $ RecFun n v (foldr funBuilder e (zip vs ts)) t
	where 
		funBuilder :: (String, Type) -> Expression -> Expression
		funBuilder (x, t) e = Fun x e t

-- | Parser for anonymous function declations.
fun :: Parser Expression
fun = do
	string "fun"
	te <- try (spaces >> inBrackets types) <|> fmap TypeVar getFreshVar
	vs <- many1 $ try $ spaces >> identifierString
	skipMany space
	string "->"
	skipMany space
	e <- expression
	skipMany space
	string "end"
	ts <- sequence $ replicate (length vs - 1) $ fmap TypeVar getFreshVar
	return $ foldr funBuilder e (zip vs (te:ts))
	where 
		funBuilder :: (String, Type) -> Expression -> Expression
		funBuilder (x, t) e = Fun x e t

-- | Parser for let bindings.
letBinding :: Parser Expression
letBinding = do
	string "let"
	te <- try (spaces >> inBrackets types) <|> fmap TypeVar getFreshVar
	spaces
	x <- identifierString
	skipMany space
	string "="
	skipMany space
	e <- expression
	skipMany space
	string "in"
	skipMany space
	r <- expression
	spaces
	string "end"
	return $ Let x e r te


-- | Parser of types.
types :: Parser Type
types = try funType <|> nonFunTypes

-- | Parser of non-function types.
nonFunTypes :: Parser Type
nonFunTypes = intType <|> boolType <|> typeVar <|> listType

-- | Parser of the int type.
intType :: Parser Type
intType = do
	string "int"
	return IntType

-- | Parser of the bool type.
boolType :: Parser Type
boolType = do
	string "bool"
	return BoolType

-- | Table of type operators.
typetable = [
		[Infix arrow AssocRight]
	]

-- | Parser of function types.
funType :: Parser Type
funType = buildExpressionParser typetable (try nonFunTypes <|> inParens types)

-- | Parser of the arrow type operation.
arrow :: Parser (Type -> Type -> Type)
arrow = try $ do
	optional spaces
	string "->"
	optional spaces
	return FunType

typeVar :: Parser Type
typeVar = do
	s <- many1 upper
	return $ TypeVar s

-- | Parser of the [ ] type.
listType :: Parser Type
listType = do
	char '['
	many space
	t <- types
	many space
	char ']'
	return $ ListType t


-- | Parses a given string into an Expression.
getExpression :: String -> Either String Expression
getExpression s = case runParser program freshVars "" s of 
	Right e -> Right e
	Left e -> Left $ show e

-- | Parses an Expression from a file.
getProgram :: String -> IO (Either String Expression)
getProgram f = do
	s <- (C.try :: IO String -> IO (Either IOError String)) $ readFile f
	return $ either (const $ Left $ "Impossible to open file " ++ f ++ ".") Right s >>= getExpression