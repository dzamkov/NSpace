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