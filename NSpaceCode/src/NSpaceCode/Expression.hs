-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..),
	getBoundVars,
	Definite(..),
	varDefinite,
	abstractDefinite,
	funcDefinite,
	patternMatch,
	splitDefiniteFunction
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map

-- A relation between variables, and functions
-- that produces a definite value.

data Expression	=	
	Variable |
	Function Expression Expression |
	ForAll (Set.Set Int) Expression |
	Fold (Set.Set Int) Expression |
	Scope (Set.Set Int) Expression deriving (Show, Ord, Eq)
	
-- Variable	: 	Declares a placeholder for a single value
-- Function	:	Applies an expression to another as a function
-- ForAll	:	True if and only if the nested expression is true for all values of the specified vars
-- Fold		:	Declares multiple vars to have the same value and act as a single variable
-- Scope		:	Hides some variables in the nested expression

-- Gets the amount of bound variables in an expression. Bound variables can be accessed with an index between
-- 0 and this number

getBoundVars						::	Expression -> Int
getBoundVars (Variable)			=	1
getBoundVars (Function x y)	=	(getBoundVars x) + (getBoundVars y)
getBoundVars (ForAll x y)		=	(getBoundVars y) - (Set.size x)
getBoundVars (Fold x y)			=	(getBoundVars y) - (Set.size x) + 1
getBoundVars (Scope x y)		=	(getBoundVars y) - (Set.size x)

-- An expression with some or all variables filled with values

data Definite a		=
	Definite Expression (Map.Map Int a) deriving (Show, Ord, Eq)
	
-- Splits a definite of a function expression into the function
-- and argument parts.
	
splitDefiniteFunction	::	Definite a -> (Definite a, Definite a)
splitDefiniteFunction (Definite (Function x y) m)	=	res
	where
		xsize	=	getBoundVars x
		xmap	=	Map.filterWithKey (\k _ -> k < xsize) m
		ymap	=	Map.fromList (map (\(k, v) -> (k - xsize, v)) (Map.toList $ Map.filterWithKey (\k _ -> k >= xsize) m))
		res	=	(Definite x xmap, Definite y ymap)
		
-- Takes a value and creates a definite variable for the value
		
varDefinite			:: a -> Definite a
varDefinite x		=	Definite (Variable) (Map.singleton 0 x)

-- Creates an abstract variable, for use in pattern matching

abstractDefinite		::	Definite a
abstractDefinite		=	Definite (Variable) (Map.empty)

-- Combines two variable maps given the size of the first variable set.

combineVarMap			::	Int -> (Map.Map Int a) -> (Map.Map Int a) -> (Map.Map Int a)
combineVarMap s x y	=	Map.union x (Map.fromList $ (map (\(k, v) -> (k + s, v)) (Map.toList y)))

-- Takes two definite expressions and applies them to each other to
-- create a function

funcDefinite	::	Definite a -> Definite a -> Definite a
funcDefinite (Definite se sm) (Definite pe pm)	=	res
	where
		res	=	Definite (Function se pe) (combineVarMap (getBoundVars se) sm pm)
	
-- Given a definite expression and a generalized expression (one with less defined variables), this will
-- try to match the pattern expression with the first expression. If sucsessful, the result will be
-- a map of variables in the pattern expression matched with the concrete value that have in the first
-- expression.
	
patternMatch	::	(Eq a, Ord a) => Definite a -> Definite a -> Maybe (Map.Map Int (Definite a))
patternMatch (Definite se sm) (Definite (Variable) pm)	=	res
	where
		res	=	if Map.member 0 pm
					then	case se of
							(Variable)	->	if 	(Map.lookup 0 sm) == (Map.lookup 0 pm)
												then 	(Just Map.empty)
												else	 Nothing
							_				->	Nothing
					else	Just (Map.singleton 0 (Definite se sm))
patternMatch (Definite (Function sef sea) sm) (Definite (Function pef pea) pm)	=	res
	where
		sizesef	=	getBoundVars sef
		splits	=	splitDefiniteFunction (Definite (Function sef sea) sm)
		splitp	=	splitDefiniteFunction (Definite (Function pef pea) pm)
		res		=	case (patternMatch (fst splits) (fst splitp), patternMatch (snd splits) (snd splitp)) of
			(Just nsm, Just npm)		->	Just (combineVarMap sizesef nsm npm)
			_								-> Nothing