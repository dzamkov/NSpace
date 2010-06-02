-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Value (
	Cons(..),
	Value(..),
	Table(..),
	SimpleTable(..),
	SimpleValue(..)
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression

-- A type usable as a constant
	
class (Eq a) => Cons a where
	equalCon		::	a
	iteCon		:: a
	andCon		:: a
	orCon			:: a
	xorCon		::	a
	notCon		::	a
	trueCon		::	a
	falseCon		::	a
	
instance Cons [Char] where
	equalCon		=	"="
	iteCon		=	"ite"
	andCon		=	"and"
	orCon			=	"or"
	xorCon		=	"xor"
	notCon		=	"not"
	trueCon		=	"true"
	falseCon		=	"false"

-- A representation of any value that can be created from the application of constants.

class (Cons b) => Value a b | a -> b where 
	isTrue				::	a -> Bool	
	valueConstant		::	a -> Maybe b		-- Constant that evaluates to this value perfectly, if one exists and is known
	

-- (Possibly infinite) table of interrelated values. Such a table can describe possible combinations
-- of values and their applications that compose another set of values.
	
class (Value b c) => Table a b c | a -> b where
	tableFree				::	a								-- Table that contains all possible values in one column
	tableColumns			::	a -> Int
	tableEmpty				::	a -> Bool
	tableValue				::	a -> Maybe [b]				--	Gets the values of the table if only one row exists
	tableFilter				::	a -> Int -> c -> a		--	Requires a column to have a certain value, removes the column
	tableApply				::	a -> Int -> Int -> a		--	Applies a column to another column as a function
	tableJoin				:: a -> Int -> Int -> a		--	Remove all rows where the specified columns are different
	tableIgnore				::	a -> Int -> a
	tableMerge				::	a -> a -> a
	

-- A simple unoptimizied implemementation of a value

data SimpleValue a =
	ConstantValue a deriving(Show)

instance (Cons a) => Value (SimpleValue a) a where
	isTrue (ConstantValue x)			=	x == trueCon
	valueConstant (ConstantValue x)	=	Just x

-- A simple unoptimizied implemenentation of a table

data SimpleTable a =
	EmptyTable Int |
	FreeTable |
	ConstantTable a deriving(Show)
	
instance (Cons a) => Table (SimpleTable a) (SimpleValue a) a where
	tableFree	=	FreeTable
	
	tableColumns (EmptyTable x)		=	x
	tableColumns (FreeTable)			=	1
	tableColumns (ConstantTable _)	=	1
	
	tableEmpty (EmptyTable _)		=	True
	tableEmpty (FreeTable)			=	False
	tableEmpty (ConstantTable _)	=	False
	
	tableValue (EmptyTable _)		=	Nothing
	tableValue (FreeTable)			=	Nothing
	tableValue (ConstantTable x)	=	Just [ConstantValue x]
	
	tableIgnore (EmptyTable x) _		=	EmptyTable (x - 1)
	tableIgnore (FreeTable) _			=	EmptyTable 0
	tableIgnore (ConstantTable _) _	=	EmptyTable 0
	
	tableMerge _ _		=	undefined