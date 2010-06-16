-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Value (
	Cons(..)
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map

-- A type usable as a constant. Only constant values may be solved for.
	
class (Eq a, Ord a) => Cons a where
	equalCons 	::	a
	andCons		::	a
	orCons		::	a
	xorCons		::	a
	xandCons		::	a
	iteCons		::	a
	trueCons		::	a
	falseCons	::	a

-- Logic and numerical constant
data SimpleCons	=
	IntegerCons (Integer) |
	LogicCons (Bool) |
	EqualCons |
	PlusCons |
	MinusCons |
	TimesCons |
	AndCons |
	OrCons |
	XorCons |
	XandCons |
	ITECons deriving (Show, Ord, Eq)
	
instance Cons SimpleCons where
	equalCons	=	EqualCons
	andCons		=	AndCons
	orCons		=	OrCons
	xorCons		=	XorCons
	xandCons		=	XandCons
	iteCons		=	ITECons
	trueCons		=	LogicCons True
	falseCons	=	LogicCons False