-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Value (
	Constant(..),
	Value(..),
	Group(..),
	SimpleGroup(..),
	SimpleValue(..),
	trueGroup
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression

-- A type usable as a constant
	
class (Eq a) => Constant a where
	equalCon		::	a
	iteCon		:: a
	andCon		:: a
	orCon			:: a
	xorCon		::	a
	notCon		::	a
	trueCon		::	a
	falseCon		::	a
	
instance Constant [Char] where
	equalCon		=	"="
	iteCon		=	"ite"
	andCon		=	"and"
	orCon			=	"or"
	xorCon		=	"xor"
	notCon		=	"not"
	trueCon		=	"true"
	falseCon		=	"false"

-- A representation of any value that can be created from the application of constants.

class (Constant b) => Value a b | a -> b where 
	isTrue				::	a -> Bool	
	valueConstant		::	a -> Set.Set b			-- Set of known single constants that equal that value exactly
	
-- Group of related values.
	
-- Filtering filters the values in the group based on related values within the group.
	
class (Value b c) => Group a b c | a -> b where
	groupSize				::	a -> Int
	groupValue				:: a -> Int -> b
	groupFilterValue		::	a -> Int -> c -> a
	groupFilterFunction	::	a -> Int -> a
	groupFilterFold		:: a -> (Set.Set Int) -> a		-- Filters values such that the specified values are equal
	groupIgnore				::	a -> Int -> a 					--	Removes a value from the group
	groupMerge				::	a -> a -> a						-- Appends one group to another
	
-- Simple unoptimized group

data SimpleGroup a	=
	ConstGroup a |
	EmptyGroup Int
	
data SimpleValue a	=
	SimpleValue Bool (Set.Set a)
	
instance (Constant a) => Value (SimpleValue a) a where
	isTrue (SimpleValue x _)				=	x
	valueConstant (SimpleValue _ x)		=	x
	
instance (Constant a) => Group (SimpleGroup a) (SimpleValue a) a where
	groupSize (ConstGroup _)						=	1
	groupSize (EmptyGroup s)						=	s
	
	
	
	groupValue (ConstGroup c) _					=	SimpleValue (c == trueCon) (Set.singleton c)
	groupValue (EmptyGroup _) _					=	undefined
	
	
	
	groupFilterValue (ConstGroup c) _ y			= 	if 	y == c
																then	ConstGroup c
																else	EmptyGroup 1
	groupFilterValue (EmptyGroup _) _ _			=	undefined
	
	
	groupFilterFunction (EmptyGroup s) _		=	EmptyGroup (s + 1)
	groupFilterFunction x y							=	undefined
	
	
	groupFilterFold (ConstGroup _) _				=	undefined
	groupFilterFold (EmptyGroup _) _				=	undefined
	
	
	
	groupIgnore (ConstGroup _) _					=	undefined
	groupIgnore (EmptyGroup s) _					=	EmptyGroup (s - 1)
	
	
	
	groupMerge _ _										=	undefined
	
-- A group with a single true value.
	
trueGroup		::	(Constant a) => SimpleGroup a
trueGroup		=	ConstGroup trueCon