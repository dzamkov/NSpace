-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..),
	implies
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Value

-- A relation between variables, and functions
-- that produces a definite value.

-- Variables are given in terms of their current
-- expression context. The context is the same
-- in both parts of a function, however, the ForAll
-- expression appends one variable to its inner context
-- and the Scope expression adds the specified amount
-- of variables to its inner context.

data Expression a	=	
	Variable Int |
	Constant a |
	Function (Expression a) (Expression a) |
	ForAll (Expression a) |
	Scope Int (Expression a) deriving (Show, Eq, Ord)
	
-- Given a true expression and a context size, this will find possible representations
-- for the specified expression.
implies	::	(Cons a) => Expression a -> Int -> Expression a -> (Set.Set (Expression a))

implies (Function (Function (Constant x) y) z) c s
	|	x == equalCons && y == s		=	(Set.union (Set.singleton s) (Set.singleton z))
	|	x == equalCons && z == s		=	(Set.union (Set.singleton s) (Set.singleton y))