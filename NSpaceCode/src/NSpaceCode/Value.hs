-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Value (
	Cons(..),
	SimpleCons(..)
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
	apply			::	a -> a -> a
	reduce		::	a -> a
	expand		::	a -> Maybe (a, a)

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
	NotCons |
	ApplyCons (SimpleCons) (SimpleCons) deriving (Show, Ord, Eq)
	
instance Cons SimpleCons where
	equalCons	=	EqualCons
	andCons		=	AndCons
	iteCons		=	ITECons
	trueCons		=	LogicCons True
	falseCons	=	LogicCons False
	notCons		=	NotCons
	apply x y	=	ApplyCons x y
	
	expand (ApplyCons x y)	=	Just (x, y)
	expand x						=	Nothing
	
	reduce (ApplyCons (ApplyCons (EqualCons) x) y)										=	LogicCons (x == y)
	reduce (ApplyCons (ApplyCons (AndCons) (LogicCons x)) (LogicCons y))			=	LogicCons (x && y)
	reduce (ApplyCons (ApplyCons (PlusCons) (IntegerCons x)) (IntegerCons y))	=	IntegerCons (x + y)
	reduce (ApplyCons (ApplyCons (MinusCons) (IntegerCons x)) (IntegerCons y))	=	IntegerCons (x - y)
	reduce (ApplyCons (ApplyCons (TimesCons) (IntegerCons x)) (IntegerCons y))	=	IntegerCons (x * y)
	reduce x																							=	x