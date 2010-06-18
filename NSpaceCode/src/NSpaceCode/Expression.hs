-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..),
	process
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
	
-- Given a context size, search expression, pattern and substitute, this will replace
-- all occurences of the pattern with the substitute.
	
substitute	::	(Cons a) => Int -> Expression a -> Expression a -> Expression a -> Expression a

substitute c x y z
	|	x == y			=	z
	|	otherwise		=	case x of
		(Function n m)	->	(Function (substitute c n y z) (substitute c m y z))
		x					->	x

-- Given a set of true expressions and the size of the context relating them. This will, fairly aimlessly figure
-- out some more expressions to put in the set. This will also attempt to remove unneeded expressions from the sets.
	
process	::	forall a. (Cons a) => (Set.Set (Expression a)) -> Int -> (Set.Set (Expression a))

process s c	=	Set.fold (processRule c) s s
	where
		processRule	::	Int -> Expression a -> (Set.Set (Expression a)) -> (Set.Set (Expression a))
		
		processRule c r@(Function (Function (Constant l) m) n) e
			|	l == andCons					=	(Set.delete r (Set.insert m (Set.insert n e)))
			|	l == equalCons					=	Set.fold (\x y -> 
															Set.insert x $
															Set.insert (substitute c x m n) $
															Set.insert (substitute c x n m) y) Set.empty e