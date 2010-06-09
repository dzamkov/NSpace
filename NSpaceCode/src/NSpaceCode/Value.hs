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
	Tree(..),
	simplify,
	evaluate,
	simplifyColumn,
	appMap
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression

-- A type usable as a constant
	
class (Eq a, Ord a) => Cons a where
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
	tableSubsect			::	(Set.Set Int) -> a -> a		--	Gets a subsection of the table, specified with a set of columns
	tableProperty			::	a -> Int -> (Map.Map Int Int) -> Int -> a -> a
	tableMerge				::	a -> a -> a						-- Note that merged tables must be given in reverse order of column appearence
	
-- A property operation combines two tables. One that defines a property a group of
-- values can have and another that uses them. The first two arguments specify a column
-- in the table that defines the abstract property. The property exists in all rows where
-- the specified column is true. The next argument is a mapping of columns from the first
-- table to the second showing the relationship they have. The last argument is a boolean column
-- in the second table. It is true only where the columns in the second table have the property
-- described in the first, if the column at the second argument is true everywhere in the first
-- table.
	

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
	PropertyTable (SimpleTable a) (SimpleTable a) Int Int (Map.Map Int Int) |
	SubsectTable (SimpleTable a) (Set.Set Int) |
	MergeTable (SimpleTable a) (SimpleTable a) deriving(Show)
	
instance (Cons a) => Table (SimpleTable a) (SimpleValue a) a where
	tableEmpty	=	EmptyTable 0

	tableFree	=	FreeTable
	
	tableColumns (EmptyTable x)		=	x
	tableColumns (FreeTable)			=	1
	tableColumns (ConstantTable _)	=	1
	tableColumns (FilterTable x _ _)	=	(tableColumns x)
	tableColumns (ApplyTable x _ _)	=	(tableColumns x) + 1
	tableColumns (JoinTable x y)		=	(tableColumns x)
	tableColumns (MergeTable x y)		=	(tableColumns x) + (tableColumns y)
	
	tableIsEmpty (EmptyTable _)		=	True
	tableIsEmpty (FreeTable)			=	False
	tableIsEmpty (ConstantTable _)	=	False
	
	tableValue (EmptyTable _) _		=	Nothing
	tableValue (FreeTable) _			=	Nothing
	tableValue (ConstantTable x) _	=	Just x
	
	tableFilter x y z			=	FilterTable z x y
	
	tableJoin x y				=	JoinTable y x
	
	tableApply x y z			=	ApplyTable z x y
	
	tableProperty v w x y z	=	PropertyTable v z w y x
	
	tableSubsect x z	=	SubsectTable z x
	
	tableMerge x y		=	MergeTable y x
	
-- Tries to reduce a table to a simpler form
simplify	::	(Cons a) => SimpleTable a -> SimpleTable a

simplify (FilterTable tab pos val)	=	res
	where
		ntab		=	simplify tab
		nnorm		=	FilterTable ntab pos val
		res		=	case ntab of
			(FreeTable)								->	ConstantTable val
			(ApplyTable intab func arg)		->	if 	pos < tableColumns intab
															then	ApplyTable (simplify (FilterTable intab pos val)) func arg
															else	case (appMap ntab (tableColumns intab)) of
				(Branch (Branch (Leaf func) (Leaf farg)) (Leaf sarg))		->	case (simplifyColumn (ntab, func)) of
					((ConstantTable (ConstantValue z)), 0)	->	if 	z == equalCon
																			then	if		getConstant val == Just trueCon
																					then	simplify (MergeTable (JoinTable intab (Set.fromList [farg, sarg])) (ConstantTable (ConstantValue trueCon)))
																					else	nnorm
																			else	nnorm
					_													-> nnorm
				_													->	nnorm
			(JoinTable intab cols)				->	if 	(Set.member pos cols)
															then	simplify (JoinTable (Set.fold (\x y -> FilterTable y x val) intab cols) cols)
															else	nnorm
			(FilterTable intab inpos inval)	->	FilterTable (simplify (FilterTable intab pos val)) inpos inval
			(MergeTable taba tabb)				->	if		pos < tableColumns taba
															then	MergeTable (simplify (FilterTable taba pos val)) tabb
															else	MergeTable taba (simplify (FilterTable tabb (pos - (tableColumns taba)) val))
			_	->	nnorm
			
simplify (JoinTable tab cols)			=	res
	where
		ntab		=	simplify tab
		nnorm		=	JoinTable ntab cols
		valmap	=	Set.map (\x -> case simplifyColumn (ntab, x) of
			((ConstantTable (ConstantValue y)), 0)	->	Just y
			_													->	Nothing) cols
		allSame				::	(Eq a) => [a] -> Bool
		allSame []			=	True
		allSame (x:[])		=	True
		allSame (x:y:s)	=	x == y && (allSame (y:s))
		
		res		=	if 	(Set.size cols) < 2
						then	ntab
						else	if 	allSame (Set.toList valmap) && head (Set.toList valmap) /= Nothing
								then	ntab
								else	nnorm

simplify (ApplyTable tab func arg)	=	res
	where
		ntab	=	simplify tab
		nnorm	=	ApplyTable ntab func arg
		
		createEvaTree	::	(Cons a) => SimpleTable a -> Tree Int -> Maybe (Tree a)
		createEvaTree x (Leaf y)	=	case simplifyColumn (x, y) of
			((ConstantTable (ConstantValue z)), 0)	->	Just (Leaf z)
			_													->	Nothing
		createEvaTree x (Branch y z)	=	case (createEvaTree x y, createEvaTree x z) of
			(Just n, Just m)		->	Just (Branch n m)
			_							-> Nothing
		
		res 	=	case (createEvaTree nnorm (appMap nnorm (tableColumns nnorm - 1))) of
			Just l	->	case (evaluate l) of
				Just a	->	MergeTable ntab (ConstantTable (ConstantValue a))
				Nothing	->	nnorm
			Nothing	->	nnorm
		
simplify (MergeTable tab (EmptyTable 0))	=	simplify tab
simplify (MergeTable (EmptyTable 0) tab)	=	simplify tab
simplify (MergeTable tab (EmptyTable s))	=	EmptyTable (s + tableColumns tab)
simplify (MergeTable (EmptyTable s) tab)	=	EmptyTable (s + tableColumns tab)
simplify (MergeTable taba tabb)				=	MergeTable (simplify taba) (simplify tabb)

simplify x	=	x

-- This will evaluate the function application of the first item applied to the second, the
-- result of that applied to the third and so on. If a single constant anwser is possible, it
-- will be returned.

data Tree a =
	Branch (Tree a) (Tree a) |
	Leaf a deriving(Show, Eq)
	
transformTree	::	(a -> b) -> Tree a -> Tree b
transformTree x (Leaf y)		=	Leaf (x y)
transformTree x (Branch y z)	=	Branch (transformTree x y) (transformTree x z)

evaluate	::	(Cons a) => Tree a -> Maybe a
evaluate (Branch (Leaf x) (Leaf y))
	|	x == notCon && y == falseCon		=	Just trueCon
	|	x == notCon && y == trueCon		=	Just falseCon
	|	otherwise								=	Nothing
evaluate _	=	Nothing

-- Simplifies a table with the priority being to simplify the specified column.

simplifyColumn	:: (Cons a) => (SimpleTable a, Int) -> (SimpleTable a, Int)
simplifyColumn ((MergeTable taba tabb), x)			=	if 	x < tableColumns taba
																		then	simplifyColumn (taba, x)
																		else	simplifyColumn (tabb, x - (tableColumns taba))
simplifyColumn ((ApplyTable subtab func arg), x)	=	if		x == tableColumns subtab
																		then	((ApplyTable subtab func arg), x)
																		else	simplifyColumn (subtab, x)
simplifyColumn (x, y)	=	(x, y)

-- Creates an application map given a table and a column in the table. An application map shows
-- what columns are applied to what other columns to create the resulting column.


appMap	::	(Cons a) => SimpleTable a -> Int -> Tree Int
appMap (ApplyTable subtab func arg) x				=	if 	x	== tableColumns subtab
																	then	Branch (appMap subtab func) (appMap subtab arg)
																	else	appMap subtab x
appMap (MergeTable taba tabb) x						=	if		x < tableColumns taba
																	then	appMap taba x
																	else	transformTree (+ (tableColumns taba)) (appMap tabb (x - tableColumns taba)) 
appMap norm@(FilterTable subtab pos val) x		=	if		x == pos
																	then	Leaf x
																	else	appMap subtab x
appMap _ y			=	Leaf y