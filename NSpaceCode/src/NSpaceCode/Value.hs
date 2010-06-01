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
	Table(..),
	SimpleTable(..),
	SimpleValue(..)
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
	valueConstant		::	a -> Maybe b		-- Constant that evaluates to this value perfectly, if one exists and is known
	

-- (Possibly infinite) table of interrelated values. Such a table can describe possible combinations
-- of values and their applications that compose another set of values.
	
class (Value b c) => Table a b c | a -> b where
	tableFree				::	a								-- Table that contains all possible values in one column
	tableColumns			::	a -> Int
	tableEmpty				::	a -> Bool
	tableValue				::	a -> Int -> Maybe b		-- Gets a value at the column, if only one row exists
	tableFilterValue		::	a -> Int -> c -> a
	tableFilterFunction	::	a -> Int -> a
	tableFilterFold		::	a -> (Set.Set Int) -> a
	tableFilterForall		::	a -> Int -> a -> Int -> a
	tableIgnore				::	a -> Int -> a
	tableMerge				::	a -> a -> a

-- Filters can be used to narrow down the possible values in a table. Filtering by function
-- will remove all rows where the specified column can not be expressed as a function application
-- of one value onto another. The specified column is ignored and the function and arguments are prepended
-- as columns to the begining of the table. Filtering by value will remove the specified column and all rows
-- where that column is not of the specified value. Filtering by fold will remove all rows where the specified
-- columns are different. Filtering by forall, perhaps the most complicated filter, works like filter by value on
-- the first table at the specified column, it filters by true if and only if the specified column of the other 
-- provided table contains all possible values and all other columns in the table are congruent at all rows. The
-- values of these extra columns are prepended to the first table.
	

-- A simple unoptimizied implemementation of a value

data SimpleValue a =
	ConstantValue a deriving(Show)

instance (Constant a) => Value (SimpleValue a) a where
	isTrue (ConstantValue x)			=	x == trueCon
	valueConstant (ConstantValue x)	=	Just x

-- A simple unoptimizied implemenentation of a table

data SimpleTable a =
	EmptyTable Int |
	FreeTable |
	ConstantTable a deriving(Show)
	
instance (Constant a) => Table (SimpleTable a) (SimpleValue a) a where
	tableFree	=	FreeTable
	
	tableColumns (EmptyTable x)		=	x
	tableColumns (FreeTable)			=	1
	tableColumns (ConstantTable _)	=	1
	
	tableEmpty (EmptyTable _)		=	True
	tableEmpty (FreeTable)			=	False
	tableEmpty (ConstantTable _)	=	False
	
	tableValue (EmptyTable _) _		=	Nothing
	tableValue (FreeTable) _			=	Nothing
	tableValue (ConstantTable x) _	=	Just (ConstantValue x)
	
	tableFilterValue (EmptyTable x) _ _		=	EmptyTable (x - 1)
	tableFilterValue (FreeTable) _ c			=	ConstantTable c
	tableFilterValue (ConstantTable x) _ v	=	res
		where
			res	=	if		v == x
						then	ConstantTable x
						else	EmptyTable 1
	
	tableFilterFunction (EmptyTable x) _		=	EmptyTable (x + 1)
	tableFilterFunction (FreeTable) _			=	tableMerge FreeTable FreeTable
	tableFilterFunction (ConstantTable _) _	=	undefined
	
	tableFilterFold (EmptyTable x) s		=	EmptyTable (x - (Set.size s) + 1)
	tableFilterFold (FreeTable) _			=	undefined
	tableFilterFold (ConstantTable _) _	=	undefined
	
	tableFilterForall _ _ _ _		=	undefined
	
	tableIgnore (EmptyTable x) _		=	EmptyTable (x - 1)
	tableIgnore (FreeTable) _			=	EmptyTable 0
	tableIgnore (ConstantTable _) _	=	EmptyTable 0
	
	tableMerge _ _		=	undefined