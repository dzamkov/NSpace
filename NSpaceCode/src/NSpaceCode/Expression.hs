-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..),
	ExpressionTable(..),
	getBoundVars,
	expToTable,
	solve,
	quickSolve
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Value

-- A relation between variables, and functions
-- that produces a definite value.

data Expression a	=	
	Variable String |
	Constant a |
	Function (Expression a) (Expression a) |
	ForAll (Set.Set String) (Expression a) |
	Scope (Set.Set String) (Expression a) |
	ListExp [Expression a] | 
	SetExp [Expression a] deriving (Show, Ord, Eq)
	
-- Variable	: 	Declares a placeholder for a single value
-- Function	:	Applies an expression to another as a function
-- ForAll	:	True if and only if the nested expression is true for all values of the specified vars
-- Fold		:	Declares multiple vars to have the same value and act as a single variable
-- Scope		:	Hides some variables in the nested expression

-- Gets the names of the variables bound in an expression.

getBoundVars						::	Expression a -> Set.Set String
getBoundVars (Variable s)		=	Set.singleton s
getBoundVars (Function x y)	=	Set.union (getBoundVars x) (getBoundVars y)
getBoundVars (ForAll v e)		=	Set.difference (getBoundVars e) v
getBoundVars (Scope v e)		=	Set.difference (getBoundVars e) v

-- Creates a value table from an expression and returns a mapping of variables
-- in it.

data ExpressionTable a	=	ExpressionTable {
	dataTable 		::	a, 
	varMap			::	(Map.Map String Int),
	resultColumn	::	Int } deriving (Show)

expToTable		:: forall a b c. (Table a b c) => Expression c -> ExpressionTable a
expToTable (Variable s)		=	ExpressionTable tableFree (Map.singleton s 0) 0
expToTable (Constant x)		=	ExpressionTable (tableFilter 0 (constantValue x) tableFree) Map.empty 0
expToTable (Function x y)	=	res
	where
		xt		=	expToTable x	::	ExpressionTable a
		yt		=	expToTable y	::	ExpressionTable a
		int	=	Map.intersectionWith (\x y -> (x, y + tableColumns (dataTable xt))) (varMap xt) (varMap yt)
		ntab	=	tableApply (resultColumn xt) (resultColumn yt + tableColumns (dataTable xt)) (Map.fold (\x y -> 
						tableJoin (Set.fromList [fst x, snd x]) y) (tableMerge (dataTable yt) (dataTable xt)) int)
		nmap	=	Map.unions [Map.map (\x -> fst x) int, varMap xt, Map.map (\x -> x + (tableColumns (dataTable xt))) (varMap yt)]
		res	=	ExpressionTable ntab nmap ((tableColumns (dataTable xt)) + (tableColumns (dataTable yt)))
		
solve			::	forall a. (Cons a) => Expression a -> String -> Maybe (SimpleValue a)
solve x y	=	res 
	where
		tab	=	(expToTable x) :: ExpressionTable (SimpleTable a)
		ntab	=	(tableFilter (resultColumn tab) (constantValue trueCon) (dataTable tab))
		res	=	case Map.lookup y (varMap tab) of
						(Just z)		->		(tableValue ntab z)
						Nothing		->		Nothing
		
quickSolve	::	forall a. (Cons a) => Expression a -> String -> Maybe a
quickSolve x y		=	case (solve x y) of
	(Just z)		->		getConstant z
	Nothing		->		Nothing