-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Value (
	Cons(..),
	SimpleCons(..),
	Value(..),
	Table(..),
	tableSize,
	tableValue
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map

-- A type usable as a constant
	
class (Eq a, Ord a) => Cons a where
	valueEqual	::	Value a -> Value a -> Bool
	
-- Logic and numerical constant
data SimpleCons	=
	IntegerCons (Integer) |
	LogicCons (Bool) |
	PlusCons |
	MinusCons |
	TimesCons |
	AndCons |
	OrCons |
	XorCons |
	XandCons |
	ITECons deriving (Show, Ord, Eq)
	
instance Cons SimpleCons where
	valueEqual (ConsValue x) (ConsValue y)	=	True
	
-- A value made from a constant or the application
-- of values onto each other as functions.
	
data Value a	=
	ConsValue a |
	FuncValue (Value a) (Value a) deriving (Show)
	
instance (Cons a) => Eq (Value a) where
	x == y		=	valueEqual x y
	
-- A table of interrelated values, possibly with an 
-- infinite amount of rows.
	
data Table a	=
	MiniTable (Value a) |
	MergeTable (Table a) (Table a) |
	FreeTable |
	ColumnTable (Table a) Int |
	JoinTable (Table a) Int Int |
	ApplyTable (Table a) Int Int deriving (Show)

instance (Cons a) => Eq (Table a) where
	MiniTable x == MiniTable y							=	x == y
	FreeTable == FreeTable								=	True
	ColumnTable (MergeTable x y) z == l				=	if		z < tableSize x
																	then	ColumnTable x z == l
																	else	ColumnTable y (z - tableSize x) == l
	ColumnTable FreeTable 0 == FreeTable			=	True
	ColumnTable (MiniTable x) 0 == MiniTable y	=	x == y
	
	
-- MiniTable 	:	Contains ONE expressible value in one column
-- MergeTable	:	Combines two tables by matching every row on one to every row on the other
-- FreeTable	:	Contains EVERY expressible value in one column
-- ColumnTable	:	Contains every value in one column the a specified table
-- JoinTable	:	Contains all the rows in the table where the specified columns are equal
-- ApplyTable	:	Appends a column at the end of the table that shows the function application of the specified rows
	
-- Gets the size in columns of a table.
tableSize	::	Table a -> Int
tableSize (MiniTable _)			=	1
tableSize (MergeTable x y)		=	(tableSize x) + (tableSize y)
tableSize (FreeTable)			=	1
tableSize (ColumnTable _ _)	=	1
tableSize (JoinTable x _ _)	=	tableSize x
tableSize (ApplyTable x _ _)	=	tableSize x + 1
	
-- Gets the value of a column in a table, if the table has exactly
-- one unique value in that column.
tableValue	::	(Cons a) => (Table a) -> Int -> Maybe (Value a)


tableValue (MiniTable x) _			=	Just x
tableValue (MergeTable x y) z		=	if		z < tableSize x
												then	tableValue x z
												else 	tableValue y (z - tableSize x)
tableValue (FreeTable) _			=	Nothing
tableValue (ColumnTable x y) _	=	tableValue x y


tableValue (JoinTable w x y) z
	|	z /= x && z /= y									=	tableValue w z
	|	z == x && ColumnTable w x == FreeTable		=	tableValue w y
	|	z == y && ColumnTable w y == FreeTable		=	tableValue w x