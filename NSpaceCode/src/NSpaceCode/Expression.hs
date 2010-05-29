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
	foldDefinite,
	unfoldDefinite,
	patternMatch,
	splitDefinite
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
	
splitDefinite	::	Definite a -> (Definite a, Definite a)
splitDefinite (Definite (Function x y) m)	=	res
	where
		xsize	=	getBoundVars x
		xmap	=	Map.filterWithKey (\k _ -> k < xsize) m
		ymap	=	Map.fromList (map (\(k, v) -> (k - xsize, v)) (Map.toList $ Map.filterWithKey (\k _ -> k >= xsize) m))
		res	=	(Definite x xmap, Definite y ymap)
		
-- Takes a definite made from a fold and gets the expression part
-- of the fold.
		
unfoldDefinite		::	Definite a -> Definite a
unfoldDefinite (Definite (Fold s e) m)	=	res
	where
		size			=	getBoundVars (Fold s e)
		val			=	Map.lookup (size - 1) m
		foldfunc				::	Maybe b -> (Map.Map Int b) -> Int -> (Int, [(Int, b)]) -> (Int, [(Int, b)])
		foldfunc v l x (y, z)	=	if 	Set.member x s
											then	case v of
														(Just w)		->	(y, (x, w):z)
														Nothing		->	(y, z)
											else	case Map.lookup y l of
														(Just w)		->	(y + 1, (x, w):z)
														Nothing		->	(y + 1, z)
		res			=	Definite e (Map.fromList $ snd $ foldr (foldfunc val m) (0, []) [0..(getBoundVars e - 1)])
		
-- Adds some variables to the variable map in a definite
		
unionDefinite							::	Definite a -> (Map.Map Int a) -> Definite a
unionDefinite (Definite e m) y	=	Definite e (Map.union m y)
		
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
		
-- Removes the specified variables from a variable map and moves the variables back as needed

removeVarMap			::	(Map.Map Int a) -> (Set.Set Int) -> (Map.Map Int a)
removeVarMap m s		=	Map.fromList $ snd $ foldr foldfunc (0, []) (Map.toList m)
	where
		foldfunc						::	(Int, a) -> (Int, [(Int, a)]) -> (Int, [(Int, a)])
		foldfunc (w, x) (y, z)	=	if		Set.member w s
											then	(y + 1, z)
											else	(y, (w - y, x):z)
		
-- Gets the variable map for a definite

getVarMap						::	Definite a -> (Map.Map Int a)
getVarMap (Definite _ m)	=	m

-- Creates a definite where the specified variables in the first definite are equal to
-- the same value at all times. The resulting variable is moved to the end of the variable
-- list. In order for the fold to be sucsessful, all the specified variables should have the
-- same value.
		
foldDefinite	::	Definite a -> (Set.Set Int) -> Definite a
foldDefinite (Definite e m) s		=	res
	where
		size	=	getBoundVars e
		nval	=	Map.lookup (Set.findMin s) m
		pmap	=	removeVarMap m s
		nmap	=	case nval of
						Just x		->	Map.insert (size - (Set.size s)) x pmap
						Nothing		->	pmap
		res	=	if		(Set.size s) > 0
					then	Definite (Fold s e) nmap
					else 	undefined
	
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
		splits	=	splitDefinite (Definite (Function sef sea) sm)
		splitp	=	splitDefinite (Definite (Function pef pea) pm)
		res		=	case (patternMatch (fst splits) (fst splitp), patternMatch (snd splits) (snd splitp)) of
			(Just nsm, Just npm)		->	Just (combineVarMap sizesef nsm npm)
			_								-> Nothing