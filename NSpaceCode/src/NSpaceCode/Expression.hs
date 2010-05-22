-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression,
	createVar,
	reallocateExpVar,
	boundExpVars,
	remapExpVars,
	mapExps,
	createFunc
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map

-- A relation between variables, and functions
-- that produces a definite value. The provided type to
-- Expression determines the type of the value it can 
-- evaluate to.

data Expression		=	
	Variable Int |
	Function Expression Expression
	
-- Creates a variable from a variable index.
	
createVar		::	Int -> Expression
createVar x		=	Variable x

-- Reallocates the variables in an expression so that they are mapped
-- continously starting at the specified index. The resulting expression
-- and first unused variable are returned.

reallocateExpVar						::	Expression -> Int -> (Expression, Int)
reallocateExpVar (Variable x) y			=	(Variable y, y + 1)
reallocateExpVar (Function a b) y		=	(Function (fst func) (fst arg), snd arg)
										where
											func	=	reallocateExpVar a y
											arg		=	reallocateExpVar b (snd func)
										
-- Gets a set of all variable indices that are bound in the expression.
										
boundExpVars					::	Expression -> (Set.Set Int)
boundExpVars (Variable x)		=	Set.singleton x
boundExpVars (Function x y)		=	Set.union (boundExpVars x) (boundExpVars y)

-- Remaps the variables in an expression based on a mapping function.

remapExpVars					::	Expression -> (Int -> Int) -> Expression
remapExpVars (Variable x) y		=	Variable (y x)
remapExpVars (Function x y) z	=	Function (remapExpVars x z) (remapExpVars y z)

-- Creates a mapping between two expression by making the first expression compatible
-- with the second. The specified function shows the mapping of the first expression
-- to the second if one exists.

mapExps				::	Expression -> Expression -> (Int -> Maybe Int) -> Expression
mapExps a b x		=	remapExpVars a ((Map.!) (snd fullmap))
					where
						expa	=	boundExpVars a
						expb	=	boundExpVars b
						foldf					::	Int -> (Int, Map.Map Int Int) -> (Int, Map.Map Int Int)
						foldf nxt (uns, map)	=	(nextun, Map.insert nxt var map)
												where
													findVar					::	(Maybe Int, Int) -> (Int, Int)
													findVar ((Just n), m)	=	(n, m)
													findVar ((Nothing), m)	=	(m, m + 1)
													fv		=	findVar (x nxt, uns)
													var		=	fst fv
													nextun	=	snd fv
						fullmap	=	(Set.fold foldf (Set.findMax expb + 1, Map.empty) expa) :: (Int, Map.Map Int Int)
						

-- Creates a function as the application of an expression on
-- another. There should be a direct relation of variables between
-- the two expression.

createFunc			::	Expression -> Expression -> Expression
createFunc x y		=	Function x y
												
												