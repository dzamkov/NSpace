-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..),
	Pattern(..),
	Instance(..),
	Cons(..),
	SimpleCons(..),
	rebind,
	getBound,
	getUsed,
	replace,
	score,
	substitute
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map

-- A type usable as a constant. Only constant values may be solved for.
class (Eq a, Ord a) => Cons a where
	equalCons 	::	a
	andCons		::	a
	iteCons		::	a
	trueCons		::	a
	falseCons	::	a
	notCons		::	a
	reduce		::	(Expression a) -> (Set.Set (Expression a))
	reduce x		=	Set.singleton x

-- Logic and numerical constant
data SimpleCons	=
	IntegerCons (Integer) |
	LogicCons (Bool) |
	CharCons (Char) |
	ListCons |
	SetCons |
	UniversalCons |
	EqualCons |
	PlusCons |
	MinusCons |
	TimesCons |
	AndCons |
	OrCons |
	XorCons |
	XandCons |
	ITECons |
	NotCons deriving (Show, Ord, Eq)
	
instance Cons SimpleCons where
	equalCons	=	EqualCons
	andCons		=	AndCons
	iteCons		=	ITECons
	trueCons		=	LogicCons True
	falseCons	=	LogicCons False
	notCons		=	NotCons

-- A relation between variables, and functions
-- that produces a definite value.

data Expression a	=	
	Variable Int |
	Constant a |
	Function (Expression a) (Expression a) |
	ForAll Int (Expression a) |
	Exists Int (Expression a) |
	Lambda Int (Expression a) | 
	Solve Int (Expression a) deriving (Show, Eq, Ord)
	
-- Expression scored and ordered by simplicity.
	
data ScoredExpression a	=	ScoredExpression (Expression a)	deriving(Show, Eq)

score	::	Expression a -> Int
score (Variable _)	=	0
score (Constant _)	=	1
score (Function x y)	=	(score x) + (score y)
score (ForAll _ x)	=	2 * (score x)
score (Exists _ x)	=	2 * (score x)
score (Lambda _ x)	=	2 * (score x)
score (Solve _ x)		=	4 * (score x)

instance (Eq a, Ord a) => Ord (ScoredExpression a) where
	compare (ScoredExpression x) (ScoredExpression y)
		|	score x > score y		=	GT
		|	score x < score y		=	LT
		|	otherwise				=	compare x y
	
-- Gets the variables bound in an expression.

getBound	::	Expression a -> (Set.Set Int)

getBound (Variable x)	=	Set.singleton x
getBound (Constant _)	=	Set.empty
getBound (Function x y)	=	Set.union (getBound x) (getBound y)
getBound (ForAll x y)	=	Set.delete x (getBound y)
getBound (Exists x y)	=	Set.delete x (getBound y)
getBound (Lambda x y)	=	Set.delete x (getBound y)
getBound (Solve x y)		=	Set.delete x (getBound y)

-- Gets the variables used in an expression.

getUsed	::	Expression a -> (Set.Set Int)
getUsed (Variable x)		=	Set.singleton x
getUsed (Constant _)		=	Set.empty
getUsed (Function x y)	=	Set.union (getUsed x) (getUsed y)
getUsed (ForAll x y)		=	Set.insert x (getUsed y)
getUsed (Exists x y)		=	Set.insert x (getUsed y)
getUsed (Lambda x y)		=	Set.insert x (getUsed y)
getUsed (Solve x y)		=	Set.insert x (getUsed y)

-- Rebinds all variables in a expression based on a mapping function

rebind	::	Expression a -> (Int -> Int) -> Expression a
rebind (Variable x) y	=	Variable (y x)
rebind (Constant x) y	=	Constant x
rebind (Function x y) z	=	Function (rebind x z) (rebind y z)
rebind (ForAll x y) z	=	ForAll (z x) (rebind y z)
rebind (Exists x y) z	=	Exists (z x) (rebind y z)
rebind (Lambda x y) z	=	Lambda (z x) (rebind y z)
rebind (Solve x y) z		=	Solve (z x) (rebind y z)
																		
-- Replaces a variable in an expression with another expression.

replace	::	Int -> Expression a -> Expression a -> Expression a
replace var to (Variable x)
	|	x == var		=	to
replace var to (Function x y)	=	Function (replace var to x) (replace var to y)
replace var to (ForAll x y)
	|	var /= x		=	ForAll x (replace var to y)
replace var to (Exists x y)
	|	var /= x		=	Exists x (replace var to y)
replace var to (Lambda x y)
	|	var /= x		=	Lambda x (replace var to y)
replace var to (Solve x y)
	|	var /= x		=	Solve x (replace var to y)
replace _ _ x		=	x

-- A pattern is an expression with some terms missing, being instead replaced
-- by a free variable.
type Pattern a = Expression a

-- Information about what "fits" into the free variables of a pattern. A pattern
-- combined with an instance can be used to generate an expression.

type Instance a = Map.Map Int (Expression a)

-- Substitutes expressions in for the free variables of a pattern based on
-- an instance.

substitute	::	(Cons a) => Pattern a -> Instance a -> Expression a
substitute p i	=	res
	where
		res	=	Map.foldWithKey (\k v b -> replace k v b) p i