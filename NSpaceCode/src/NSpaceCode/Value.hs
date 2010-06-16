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
	reduce		::	Value a -> Value a
	split			::	Value a -> Table a
	
--	reduce	:	Optionally reduces a value to a simpiler form
-- split		:	Creates a two column table showing all ways a value can 
--					be represented by the function application of two other values.	
	
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
	
	reduce 
		(FuncValue 
			(FuncValue 
				(ConsValue (PlusCons)) 
				(ConsValue (IntegerCons x))) 
			(ConsValue (IntegerCons y)))			=	ConsValue $ IntegerCons $ x + y
			
	split (ConsValue (LogicCons True))			=	MergeTable 
																(MiniTable $ FuncValue (ConsValue AndCons) (ConsValue $ LogicCons True))
																(MiniTable $ ConsValue $ LogicCons True)
	
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
tableValue	::	forall a. (Cons a) => (Table a) -> Int -> Maybe (Value a)


tableValue (MiniTable x) 0			=	Just x
tableValue (MergeTable x y) z		=	if		z < tableSize x
												then	tableValue x z
												else 	tableValue y (z - tableSize x)
tableValue (FreeTable) 0			=	Nothing
tableValue (ColumnTable x y) 0	=	tableValue x y

tableValue (JoinTable w x y) z
	|	x == y													=	tableValue w z
	|	z == x && (
		case (tableValue w x) of
			(Just n)	->	tableHasValue w y n
			Nothing	->	False)								=	tableValue w x
	|	z == y && (
		case (tableValue w y) of
			(Just n)	->	tableHasValue w x n
			Nothing	->	False)								=	tableValue w y
tableValue tab@(JoinTable (ApplyTable t l m) x y) z
	|	x < tableSize t && y < tableSize t			=	tableValue (ApplyTable (JoinTable t x y) l m) z
	|	x == tableSize t && (hasres $ getres y)	=	case getres y of (Just h) -> h
	|	y == tableSize t && (hasres $ getres x)	=	case getres x of (Just h) -> h
	where
		hasres				::	Maybe x -> Bool
		hasres (Just _)	=	True
		hasres (Nothing)	=	False
		
		-- If one joined var is the result of the apply table, this will try to get the result of
		-- the tableValue, given the other joined var
		getres		::	Int -> Maybe (Maybe (Value a))
		getres x		=	case (tableValue tab x) of
								Just n	->	Just (tableValue 
									(ApplyTable
										(JoinTable
											(JoinTable
												(MergeTable t (split n))
												l (tableSize t))
											m (tableSize t + 1))
										l m) 	(if	z == tableSize t
												then 	z + 2
												else	z))
								Nothing	->	Nothing
				

tableValue (ApplyTable w x y) z
	|	z == tableSize w && 
		tableValue w x /= Nothing && 
		tableValue w y /= Nothing						=	case (tableValue w x, tableValue w y) of
																		(Just l, Just m) -> Just (FuncValue l m)
	|	otherwise											=	tableValue w z
	
	
-- Gets if the value exists in the specified column of a table.
	
tableHasValue	::	forall a. (Cons a) => (Table a) -> Int -> Value a -> Bool

tableHasValue (MiniTable x) 0 y			=	x == y
tableHasValue (FreeTable) 0 y				=	True
tableHasValue (MergeTable w x) y z		=	if		y < tableSize w
														then 	tableHasValue w y z
														else 	tableHasValue x (y - tableSize w) z
tableHasValue (ColumnTable x y) 0 z		=	tableHasValue x y z
tableHasValue (ApplyTable t l m) x y
	|	x	< tableSize t						=	tableHasValue t x y