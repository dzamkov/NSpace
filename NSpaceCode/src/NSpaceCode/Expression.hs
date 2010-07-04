-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..),
	Cons(..),
	SimpleCons(..),
	Value(..),
	rebind,
	getBound,
	replace,
	score,
	process
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

-- Rebinds all variables in a expression based on a mapping function

rebind	::	Expression a -> (Int -> Int) -> Expression a
rebind (Variable x) y	=	Variable (y x)
rebind (Constant x) y	=	Constant x
rebind (Function x y) z	=	Function (rebind x z) (rebind y z)
rebind (ForAll x y) z	=	ForAll x (rebind y (\l ->	if		l == x
																		then	x
																		else	z l))
rebind (Exists x y) z	=	Exists x (rebind y (\l ->	if		l == x
																		then	x
																		else	z l))
rebind (Lambda x y) z	=	Lambda x (rebind y (\l ->	if		l == x
																		then	x
																		else	z l))
rebind (Solve x y) z		=	Solve x (rebind y (\l ->	if		l == x
																		then	x
																		else	z l))
																		
-- Replaces a variable in an expression with another expression.

replace	::	Int -> Expression a -> Expression a -> Expression a
replace var to (Variable x)
	|	x == var		=	to
replace var to (Function x y)	=	Function (replace var to x) (replace var to y)
replace var to (ForAll x y)
	|	var /= x		=	ForAll x (replace var to y)
replace var to (Exists x y)
	|	var /= x		=	ForAll x (replace var to y)
replace var to (Lambda x y)
	|	var /= x		=	ForAll x (replace var to y)
replace var to (Solve x y)
	|	var /= x		=	ForAll x (replace var to y)
replace _ _ x		=	x

-- Uniquely represents a value as a set of expressions that are equivalent when
-- the given axioms are true. The expressions are given without a context, and
-- therfore, should not contain any variables.

data Value a = Value {
	expression	::	Expression a,		--	An expression
	axiom			::	Expression a }	deriving(Show)
	
-- Finds expressions equivalent to a value in the value's context. Note that not
-- all possible expressions may be found in one iteration (in fact, its impossible)
-- and simpiler expressions are more likely to be found (so a better name for this
-- function would be reduce, but I'm not calling it that because it sounds so
-- definite and confident, with a clear purpose, which is certainly not this function,
-- which aimlessly guesses new expressions given old ones). Also, the reason this 
-- comment is so long is because I'm on a plane right now, and am trying
-- desperately to procastinate. There appears to be inflight wifi, but it costs
-- $12.99 per flight, and thats just stupid and I'm not having that. When I land, i'll
-- probably make a program that downloads wikipedia articles and provides a local
-- server to view them on. At least that way, i'd have some way to procastinate. I think
-- im over it now though, time to write the actual function...

-- here I go...

-- now?

-- NOW
	
process	::	(Cons a) => Value a -> Set.Set (Expression a)

process (Value e (Constant l))
	|	l == trueCons		=	Set.empty
	|	l == falseCons		=	error	$	"WTF, The definition of axiom is that it has"
											++	"to be true. NOOB"