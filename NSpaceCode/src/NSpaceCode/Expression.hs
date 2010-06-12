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
	Variable String |
	Function Expression Expression |
	ForAll (Set.Set String) Expression |
	Scope (Set.Set String) Expression |
	ListExp [Expression] | 
	SetExp [Expression] deriving (Show, Ord, Eq)
	
-- Variable	: 	Declares a placeholder for a single value
-- Function	:	Applies an expression to another as a function
-- ForAll	:	True if and only if the nested expression is true for all values of the specified vars
-- Fold		:	Declares multiple vars to have the same value and act as a single variable
-- Scope		:	Hides some variables in the nested expression

-- Gets the names of the variables bound in an expression.

getBoundVars						::	Expression -> Set.Set String
getBoundVars (Variable s)		=	Set.singleton s
getBoundVars (Function x y)	=	Set.union (getBoundVars x) (getBoundVars y)
getBoundVars (ForAll v e)		=	Set.difference (getBoundVars e) v
getBoundVars (Scope v e)		=	Set.difference (getBoundVars e) v