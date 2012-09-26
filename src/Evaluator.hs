
-- | This module is a simple evaluator of expressions.
module Evaluator(run, Value) where

import qualified Data.Map as Map
import Data.List (intersperse)
import LiPL

-- * Environments

type Environment = Map.Map Id Value

-- * Values

-- | Values express the result of an evaluation.
data Value = IntValue Int   -- ^ Integer value.
		   | BoolValue Bool -- ^ Boolean value.
		   | ListValue [Value] -- ^ List value.
		   | Closure Environment Id Expression  -- ^ Closure.

instance Show Value where
	show (IntValue n) = show n
	show (BoolValue b) = if b then "true" else "false"
	show (ListValue vs) = "[" ++ (concat $ intersperse ", " $ map show vs) ++ "]"
	show (Closure _ _ _) = "fun ... -> ... end"

-- * Evaluation

-- | Applies a binary operator on two values.
applyBinaryOperator :: String -> Value -> Value -> Either String Value
applyBinaryOperator "+" (IntValue n) (IntValue m) = return $ IntValue $ n + m
applyBinaryOperator "-" (IntValue n) (IntValue m) = return $ IntValue $ n - m
applyBinaryOperator "*" (IntValue n) (IntValue m) = return $ IntValue $ n * m
applyBinaryOperator "/" (IntValue n) (IntValue 0) = Left "Division by zero"
applyBinaryOperator "/" (IntValue n) (IntValue m) = return $ IntValue $ n `div` m
applyBinaryOperator "<" (IntValue n) (IntValue m) = return $ BoolValue $ n < m
applyBinaryOperator ">" (IntValue n) (IntValue m) = return $ BoolValue $ n > m
applyBinaryOperator "=" (IntValue n) (IntValue m) = return $ BoolValue $ n == m
applyBinaryOperator "&" (BoolValue a) (BoolValue b) = return $ BoolValue $ a && b
applyBinaryOperator "|" (BoolValue a) (BoolValue b) = return $ BoolValue $ a || b
applyBinaryOperator _ _ _ = Left "Unknown operator"  -- Should never happen

-- | Applies a unary operator on a value.
applyUnaryOperator :: String -> Value -> Either String Value
applyUnaryOperator "\\" (BoolValue b) = return $ BoolValue $ not b
applyUnaryOperator _ _ = Left "Unknown operator"  -- Should never happen

-- | Runs an expression and gets its value.
run :: Expression -> Either String Value
run = eval Map.empty

-- | Evaluates an expression given an environment.
eval :: Environment -> Expression -> Either String Value
eval env (Fun x b _) = return (Closure env x b)
eval env (RecFun f x b _) = return (Closure env' x b)
	where env' = Map.insert f (Closure env' x b) env
eval _ (Number n) = return (IntValue n)
eval _ (Boolean b) = return (BoolValue b)
eval env (List h t) = do
	vh <- eval env h
	(ListValue ts) <- eval env t
	return $ ListValue $ vh:ts
eval _ Nil = return $ ListValue []
eval env (IsNil l) = do
	(ListValue xs) <- eval env l
	return $ BoolValue $ case xs of
		[] -> True
		_  -> False
eval env (Head l) = do
	(ListValue xs) <- eval env l
	case xs of
		[] -> Left "Called head on empty list."
		(x:_) -> return x
eval env (Tail l) = do
	(ListValue xs) <- eval env l
	case xs of
		[] -> Left "Called tail on empty list."
		(_:ys) -> return $ ListValue ys
eval env (Binary c a b) = do
	va <- eval env a
	vb <- eval env b
	applyBinaryOperator c va vb
eval env (Unary c a) = do
	va <- eval env a
	applyUnaryOperator c va
eval env (Identifier x) = case Map.lookup x env of
	Nothing -> Left $ "No value for identifier " ++ x ++ "."  -- Should never happen
	Just v -> return v
eval env (IfThenElse i t e) = do
	(BoolValue vi) <- eval env i
	if vi then
		eval env t
	else
		eval env e
eval env (Application f a) = do
	(Closure env' x b) <- eval env f
	va <- eval env a
	let env'' = Map.insert x va env'
	eval env'' b
eval env (Let x a b _) = do
	va <- eval env a
	let env' = Map.insert x va env
	eval env' b