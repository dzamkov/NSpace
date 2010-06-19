-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..),
	substitute,
	implies
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Value

-- A relation between variables, and functions
-- that produces a definite value.

data Expression a	=	
	Variable Int |
	Constant a |
	Function (Expression a) (Expression a) |
	ForAll Int (Expression a) |
	Scope (Set.Set Int) (Expression a) deriving (Show, Eq, Ord)
	
-- Given a context size, search expression, pattern and substitute, this will replace
-- all occurences of the pattern with the substitute.
	
substitute	::	(Cons a) => Int -> Expression a -> Expression a -> Expression a -> Expression a

substitute c x y z
	|	x == y			=	z
	|	otherwise		=	case x of
		(Function n m)	->	(Function (substitute c n y z) (substitute c m y z))
		x					->	x 
		
		
-- Given a context size, a target expression and a statement (true expression) in the
-- same context. This will find alternate forms of the target expression based on
-- the statement.
		
implies		::	(Cons a) => Int -> Expression a -> Expression a -> (Set.Set (Expression a))

implies c t (Function (Function (Constant l) m) n)
	|	l == andCons			=	(Set.union (implies c t m) (implies c t n))
	|	l == equalCons			=	Set.fromList [substitute c t n m, substitute c t m n, t]