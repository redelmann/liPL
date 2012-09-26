
-- | This module contains the definition of the expressions of Simpl.
module LiPL(Id, Operator, Expression(..), Type(..), TypeVar) where

import Data.List (intersperse)

-- * Expressions.

-- | Identifies an identifier.
type Id = String

-- | Identifies an operator.
type Operator = String

-- | All expressions of liPL.
data Expression = Identifier Id -- ^ Identifier.
				| Number Int    -- ^ Natural number.
				| Boolean Bool  -- ^ Boolean.
				| List Expression Expression -- ^ Non-empty list.
				| Nil  -- ^ Empty list.
				| Head Expression  -- ^ Head of a list.
				| Tail Expression  -- ^ Tail of a list.
				| IsNil Expression -- ^ Tests if a list is empty.
				| Binary Operator Expression Expression  -- ^ Binary operator.
				| Unary Operator Expression  -- ^ Unary operator.
				| Fun Id Expression Type -- ^ Function definition.
				| RecFun Id Id Expression Type  -- ^ Recursive function definition.
				| Application Expression Expression  -- ^ Application.
				| IfThenElse Expression Expression Expression  -- ^ Conditional structure.
				| Let Id Expression Expression Type -- ^ Let expression.
				deriving (Eq)

instance Show Expression where
	show = showIndent 0

indents :: Int -> String
indents i = replicate i ' '

showIndent :: Int -> Expression -> String
showIndent i (Identifier x) = indents i ++ x
showIndent i (Number n) = indents i ++ show n
showIndent i (Boolean b) = indents i ++ if b then "true" else "false"
showIndent i (Binary c a b) = indents i ++ show a ++ " " ++ c ++ " " ++ show b
showIndent i (Unary c a) = indents i ++ c ++ show a
showIndent i (IfThenElse c a b) = indents i ++ "if " ++ show c ++ " then " ++
								  show a ++ " " ++
								  " else " ++
								  show b ++ " " ++
								  "end"
showIndent i Nil = indents i ++ "[]"
showIndent i (List h t) = indents i ++ show h ++ " :: " ++ show t
showIndent i (Head l) = indents i ++ "(head " ++ show l ++ ")"
showIndent i (Tail l) = indents i ++ "(tail " ++ show l ++ ")"
showIndent i (IsNil l) = indents i ++ "(empty " ++ show l ++ ")"
showIndent i (Fun x b t) = indents i ++ "fun {" ++ show t ++ "} " ++ x ++ " -> " ++ 
						   show b ++ " end"
showIndent i (RecFun f x b t) = indents i ++ "recfun " ++ f ++ " {" ++ show t ++ "} " ++ x ++ " -> " ++ 
						   show b ++ " end"
showIndent i (Application f a) = indents i ++ "(" ++ show f ++ " " ++ show a ++ ")"
showIndent i (Let x a b ta) = indents i ++ "let {" ++ show ta ++ "} " ++ x ++ " = " ++
								 show a ++ "\n" ++
								 indents i ++ "in\n" ++
								 showIndent (i+1) b	++ "\n" ++
								 indents i ++ "end"

-- * Types.

-- | Identifies a type variable.
type TypeVar = String

-- | All types of expressions.
data Type = IntType  -- ^ Integer type.
		  | BoolType -- ^ Boolean type.
		  | FunType Type Type  -- ^ Function type.
		  | ListType Type  -- ^ List type.
		  | TypeVar TypeVar  -- ^ Type variable.
		  deriving (Eq, Ord)

instance Show Type where
	show IntType = "int"
	show BoolType = "bool"
	show (FunType a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
	show (ListType t) = "[" ++ show t ++ "]"
	show (TypeVar v) = v
