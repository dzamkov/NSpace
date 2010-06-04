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
	SimpleValue(..),
	reduceTable
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
	constantValue		::	b -> a				-- Constructs a value from a single constant
	getConstant			::	a -> Maybe b		-- Constant that evaluates to this value perfectly, if one exists and is known
	

-- (Possibly infinite) table of interrelated values. Such a table can describe possible combinations
-- of values and their applications that compose another set of values.
	
class (Value b c) => Table a b c | a -> b where
	tableEmpty				::	a								-- Empty table with 0 columns and rows
	tableFree				::	a								-- Table that contains all possible values in one column
	tableColumns			::	a -> Int
	tableIsEmpty			::	a -> Bool
	tableValue				::	a -> Int -> Maybe b			--	Gets the value of a column the table if not empty, and the same at all rows
	tableFilter				::	Int -> b -> a -> a			--	Requires a column to have a certain value
	tableApply				::	Int -> Int -> a -> a			--	Applies a column to another column as a function
	tableJoin				::	(Set.Set Int) -> a -> a		--	Remove all rows where the specified columns are different
	tableIgnore				::	Int -> a -> a
	tableMerge				::	a -> a -> a
	

-- A simple unoptimizied implemementation of a value

data SimpleValue a =
	ConstantValue a deriving(Show)

instance (Cons a) => Value (SimpleValue a) a where
	constantValue x					=	ConstantValue x
	getConstant (ConstantValue x)	=	Just x

-- A simple unoptimizied implemenentation of a table

data SimpleTable a =
	EmptyTable Int |
	FreeTable |
	ConstantTable (SimpleValue a) |
	FilterTable (SimpleTable a) Int (SimpleValue a) |
	ApplyTable (SimpleTable a) Int Int |
	JoinTable (SimpleTable a) (Set.Set Int) |
	IgnoreTable (SimpleTable a) Int |
	MergeTable (SimpleTable a) (SimpleTable a) deriving(Show)
	
instance (Cons a) => Table (SimpleTable a) (SimpleValue a) a where
	tableEmpty	=	EmptyTable 0

	tableFree	=	FreeTable
	
	tableColumns (EmptyTable x)		=	x
	tableColumns (FreeTable)			=	1
	tableColumns (ConstantTable _)	=	1
	tableColumns (FilterTable x _ _)	=	(tableColumns x) - 1
	tableColumns (ApplyTable x _ _)	=	(tableColumns x) + 1
	tableColumns (JoinTable x y)		=	(tableColumns x) - (Set.size y) + 1
	tableColumns (IgnoreTable x _)	=	(tableColumns x) - 1
	tableColumns (MergeTable x y)		=	(tableColumns x) + (tableColumns y)
	
	tableIsEmpty (EmptyTable _)		=	True
	tableIsEmpty (FreeTable)			=	False
	tableIsEmpty (ConstantTable _)	=	False
	
	tableValue (EmptyTable _) _		=	Nothing
	tableValue (FreeTable) _			=	Nothing
	tableValue (ConstantTable x) _	=	Just x
	
	tableFilter x y z		=	FilterTable z x y
	
	tableJoin x y			=	JoinTable y x
	
	tableApply x y z		=	ApplyTable z x y
	
	tableIgnore _ (EmptyTable x)		=	EmptyTable (x - 1)
	tableIgnore _ (FreeTable)			=	EmptyTable 0
	tableIgnore _ (ConstantTable _)	=	EmptyTable 0
	tableIgnore y x						=	IgnoreTable x y
	
	tableMerge x y		=	MergeTable x y
	
-- Tries to reduce a table to a simpler form
	
reduceTable		::	(Cons a) => SimpleTable a -> SimpleTable a

reduceTable x	=	x
		