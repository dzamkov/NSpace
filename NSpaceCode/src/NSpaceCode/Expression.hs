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
	rebind,
	getBound,
	replace
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

