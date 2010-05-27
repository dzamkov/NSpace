-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Value (
	Value(..),
	createTrueValue,
	SimpleValue(..),
	getExpressionValue,
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression

-- Represents a value, which is represented by an infinite set
-- of all expressions that are equal to each other. Where expressions
-- are representations of values, this is the actual value.

class (Reference b) => Value a b | a -> b where
	matchValue		::	a -> Pattern b c -> c -> a				-- Creates another value that represents a flexible variable in a pattern match
	valueVariable	::	a -> (Set.Set b)							-- Variables known to equal this value
	
data SimpleValue a	=
	TruthValue (SimpleValue a) (SimpleValue a) |	-- Declares a value known to be true along with the value that actually represents this value
	MultiValue (SimpleValue a) (SimpleValue a) |	-- Declares both values to be equal to this value and to be true
	ExpValue (Expression a)								-- Declares an expression to equal this value
	
instance (Reference a) => Value (SimpleValue a) a where
	
-- Given an expression defined in a value, this will return the value the expression has
-- in the first value.
	
getExpressionValue		::	(Reference b, Value a b) => a -> Expression b -> a
getExpressionValue x y	=	matchValue x eqpattern ()
								where
									eqpattern	=	createFunc (createFunc (createVar (Definite $ equalRef)) (toPattern y)) (createVar $ Flexible ())									
	
-- Creates a true value with the information given in the true expression.
	
createTrueValue		::	Expression a -> SimpleValue a
createTrueValue x		=	ExpValue x