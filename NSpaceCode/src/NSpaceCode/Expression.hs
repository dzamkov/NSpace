-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..),
	Program(..),
	createProgram
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Value

-- A relation between variables, and functions
-- that produces a definite value.

data Expression a	=	
	Variable |
	Constant a |
	Function (Expression a) (Expression a) (Map.Map Int Int) |
	ForAll (Set.Set Int) (Expression a) |
	Scope (Set.Set Int) (Expression a) deriving (Show, Ord, Eq)
	
-- A list of expressions known to be true based on previous expressions.
	
data Program a	= Program {
	statements 	::	[(Int -> Maybe Int, Expression a)] }
	
-- Creates a program, given a true expression.
createProgram		::	Expression a -> Program a
createProgram y	=	Program [((\x -> x), y)]