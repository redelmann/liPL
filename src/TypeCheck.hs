
-- | This module is in charge of the type inference and checking of
-- expressions. The algorithm used for this is the Hindley-Milner algorithm.
module TypeCheck(inferCheck) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Monad.Error
import Data.List (intersperse, (\\))
import LiPL

-- * Substituable

-- | A type is substituable if it supports substitution of types,
-- and retrieval of its type variables.
class Substituable a where
	substitute :: Substitution -> a -> a
	freeVars :: a -> [TypeVar]

-- | An expression is substituable.
instance Substituable Expression where
	substitute s (Fun x e t) = Fun x (substitute s e) (substitute s t)
	substitute s (RecFun f x e t) = RecFun f x (substitute s e) (substitute s t)
	substitute s (Let x a b ta) = Let x (substitute s a) (substitute s b) (substitute s ta)
	substitute s (Binary c a b) = Binary c (substitute s a) (substitute s b)
	substitute s (Unary c a) = Unary c (substitute s a)
	substitute s (List h t) = List (substitute s h) (substitute s t)
	substitute s (Head l) = Head (substitute s l)
	substitute s (Tail l) = Tail (substitute s l)
	substitute s (IsNil l) = IsNil (substitute s l)
	substitute s (Application a b) = Application (substitute s a) (substitute s b)
	substitute s (IfThenElse i t e) = IfThenElse (substitute s i) (substitute s t) (substitute s e)
	substitute _ e = e

	freeVars _ = undefined


instance Substituable Type where
	substitute _ IntType = IntType
	substitute _ BoolType = BoolType
	substitute s (FunType a b) = FunType (substitute s a) (substitute s b)
	substitute s (ListType t) = ListType (substitute s t)
	substitute s (TypeVar x) = case Map.lookup x s of
		Just y -> y
		Nothing -> TypeVar x

	freeVars (TypeVar v) = [v]
	freeVars (FunType f t) = freeVars f ++ freeVars t
	freeVars (ListType t) = freeVars t
	freeVars _ = []

-- | Returns the argument type and return type of a binary operator.
typesForBinaryOperator :: String -> (Type, Type)
typesForBinaryOperator s
	| s `elem` ["+", "-", "*", "/"] = (IntType, IntType)
	| s `elem` ["&", "|"] = (BoolType, BoolType)
	| s `elem` ["<", ">", "="] = (IntType, BoolType)
	| otherwise = error "Unkown binary operator."  -- Should never happen

-- | Returns the argument type and return type of a unary operator.
typesForUnaryOperator :: String -> (Type, Type)
typesForUnaryOperator s
	| s `elem` ["\\"] = (BoolType, BoolType)
	| otherwise = error "Unkown unary operator."  -- Should never happen

-- * Schemes

data Scheme = Forall [TypeVar] Type
	deriving (Eq)

instance Show Scheme where
	show (Forall [] t) = show t
	show (Forall xs t) = "forall " ++ (concat $ intersperse " " xs) ++ ". " ++ show t

instance Substituable Scheme where
	substitute s (Forall xs t) = Forall xs (substitute s t)

	freeVars (Forall xs t) = freeVars t \\ xs

-- | Returns the Scheme of a non generalized type.
mono :: Type -> Scheme
mono t = Forall [] t

-- | Generalize a type given an Environment.
generalize :: Environment -> Type -> Scheme
generalize e t = Forall (freeVars t \\ freeVars e) t

-- | Instanciate a Scheme with fresh type variables.
instanciate :: Scheme -> FVState Type
instanciate (Forall xs t) = do
	subst <- fmap Map.fromList $ sequence $ zipWith f xs $ repeat getFreshTypeVar
	return $ substitute subst t
	where 
		f x v = do
			y <- v
			return (x, TypeVar y)

-- * Substitutions

type Substitution = Map.Map TypeVar Type

-- | Composition of substitutions.
compose :: Substitution -> Substitution -> Substitution
compose s t = Map.fromList $
	[(x, substitute s y) | (x, y) <- Map.toList t] ++ 
	[(x, y) | (x, y) <- Map.toList s, not $ x `Map.member` t]

-- * Environments

newtype Environment = Environment (Map.Map Id Scheme)
	deriving (Show)

instance Substituable Environment where
	substitute s (Environment m) = Environment $ fmap (substitute s) m

	freeVars (Environment m) = do
		s <- Map.elems m
		freeVars s

-- | Gets a Scheme from the Environment.
getScheme :: Environment -> Id -> FVState Scheme
getScheme (Environment e) x = case Map.lookup x e of
	Just s -> return s
	Nothing -> fail $ "Variable " ++ x ++ " not in the environment."

-- | Puts a Scheme from the Environment.
addScheme :: Environment -> Id -> Scheme -> Environment
addScheme (Environment e) x s = Environment $ Map.insert x s e

-- * Constraints

type Constraint = (Type, Type)

-- * Computations

-- ** Monadic definition

-- | Monad with a list of fresh type variables
-- and possible errors.
type FVState a = ErrorT String (State [TypeVar]) a

-- ** Fresh variables

-- | Returns a fresh type variable.
getFreshTypeVar :: FVState TypeVar
getFreshTypeVar = do
	(x:xs) <- lift get
	lift $ put xs
	return x

-- | Infinite list of fresh type variables.
initialFreshTypeVars :: [TypeVar]
initialFreshTypeVars = map ((:) 'a' . show) [0..]

-- * Type inference

-- | The W algorithm with constraints.
wc :: Environment -> Expression -> FVState (Type, [Constraint])
wc e (Identifier x) = do
	s <- getScheme e x
	t <- instanciate s
	return (t, [])
wc _ (Number n) = return (IntType, [])
wc _ (Boolean b) = return (BoolType, [])
wc e (List h t) = do
	(ht, hc) <- wc e h
	(tt, tc) <- wc e t
	let c' = hc ++ tc ++ [(ListType ht, tt)]
	return (tt, c')
wc e (Tail l) = do
	v <- fmap TypeVar getFreshTypeVar
	(t, c) <- wc e l
	let c' = c ++ [(ListType v, t)]
	return (t, c')
wc e (Head l) = do
	v <- fmap TypeVar getFreshTypeVar
	(t, c) <- wc e l
	let c' = c ++ [(ListType v, t)]
	return (v, c')
wc e Nil = do
	v <- fmap TypeVar getFreshTypeVar
	return (ListType v, [])
wc e (IsNil l) = do
	v <- fmap TypeVar getFreshTypeVar
	(t, c) <- wc e l
	let c' = c ++ [(ListType v, t)]
	return (BoolType, c')
wc e (Fun x b wt) = do
	v <- fmap TypeVar getFreshTypeVar
	let e' = addScheme e x (mono v)
	(t, c) <- wc e' b
	let c' = c ++ [(wt, FunType v t)]
	return (wt, c')
wc e (RecFun f x b wt) = do
	tf <- fmap TypeVar getFreshTypeVar
	let e' = addScheme e f (mono wt)
	v <- fmap TypeVar getFreshTypeVar
	let e'' = addScheme e' x (mono v)
	(t, c) <- wc e'' b
	let c' = c ++ [(FunType v t, tf), (tf, wt)]
	return (wt, c')
wc e (Application f a) = do
	(tf, cf) <- wc e f
	(ta, ca) <- wc e a
	x <- fmap TypeVar getFreshTypeVar
	let c' = cf ++ ca ++ [(tf, FunType ta x)]
	return (x, c')
wc e (Let x a b te) = do
	(ta, ca) <- wc e a
	let ca' = (ca ++ [(ta, te)]) 
	case unify ca' of
		Left e -> fail e
		Right s -> do
			let t = substitute s ta
			let e' = substitute s e
			let sc = generalize e' t
			let e'' = addScheme e' x sc
			(rt, c) <- wc e'' b
			let c' = c ++ [(te, t)] ++ ca'
			return (rt, c')
wc e (Binary c a b) = do
	let (tf, tt) = typesForBinaryOperator c
	(ta, ca) <- wc e a
	(tb, cb) <- wc e b
	let c' = ca ++ cb ++ [(ta, tf), (tb, tf)]
	return (tt, c')
wc e (Unary c a) = do
	let (tf, tt) = typesForUnaryOperator c
	(ta, ca) <- wc e a
	let c' = ca ++ [(ta, tf)]
	return (tt, c')
wc e (IfThenElse c a b) = do
	(tc, cc) <- wc e c
	(ta, ca) <- wc e a
	(tb, cb) <- wc e b
	let c' = cc ++ ca ++ cb ++ [(ta, tb), (tc, BoolType)]
	return (ta, c')

-- | Unification algorithm, takes a list of constraint,
-- and, if possible, return an equivalent substitution.
unify :: [Constraint] -> Either String Substitution
unify [] = return Map.empty
unify ((s, t):cs)
	| s == t = unify cs
	| TypeVar x <- s, not $ x `elem` freeVars t = bind x t
	| TypeVar x <- t, not $ x `elem` freeVars s = bind x s
	| FunType s1 s2 <- s
	, FunType t1 t2 <- t = unify (cs ++ [(s1, t1), (s2, t2)])
	| ListType t1 <- s
	, ListType t2 <- t = unify (cs ++ [(t1, t2)])
	| otherwise = Left $ "Type error, " ++ show s ++ " not equal to " ++ show t ++ "."
	where
		bind :: TypeVar -> Type -> Either String Substitution
		bind x a = do
			subst <- unify (constraintSubstitute cs x a) 
			return $ compose subst (Map.singleton x a)

		constraintSubstitute :: [Constraint] -> TypeVar -> Type -> [Constraint]
		constraintSubstitute c a b = map (\(x, y) -> (substi a b x, substi a b y)) c

		substi :: TypeVar -> Type -> Type -> Type
		substi _ _ IntType = IntType
		substi _ _ BoolType = BoolType
		substi x t (ListType a) = ListType (substi x t a)
		substi x t (FunType a b) = FunType (substi x t a) (substi x t b)
		substi x t (TypeVar a) = if a == x then t else TypeVar a


-- | Gets the list of type constraints from an expression.
getConstraints :: Expression -> Either String [Constraint]
getConstraints expr = fmap snd $ evalState (runErrorT (wc (Environment Map.empty) expr)) initialFreshTypeVars

-- * Type checking

-- | Given an expression, returns this expression with types infered
-- if the expression is well-typed.
inferCheck :: Expression -> Either String ([Constraint], Substitution, Expression)
inferCheck e = do
	cs <- getConstraints e
	sb <- unify cs
	return (cs, sb, substitute sb e)