-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..),
	SolverState(..),
	initSolver,
	solve,
	process,
	substitute
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
	
-- State of a solver at any give time.

data SolverState a	=	SolverState {
		context			::	Int,
		targetExps		::	(Set.Set (Expression a)),
		statementExps	::	(Set.Set (Expression a)),
		subStates		::	[SolverState a]
	} deriving (Show)
	
addTarget			::	(Cons a) => Expression a -> SolverState a -> SolverState a
addTarget m l	=	(SolverState 
	(context l) 
	(Set.insert m (targetExps l))
	(statementExps l)
	(subStates l))
	
addStatement		::	(Cons a) => Expression a -> SolverState a -> SolverState a
addStatement m l	=	(SolverState 
	(context l) 
	(targetExps l) 
	(Set.insert m (statementExps l)) 
	(subStates l))
	
removeStatement	::	(Cons a) => Expression a -> SolverState a -> SolverState a
removeStatement m l	=	(SolverState 
	(context l) 
	(targetExps l) 
	(Set.delete m (statementExps l)) 
	(subStates l))
	
addSubState			::	(Cons a) => SolverState a -> SolverState a -> SolverState a
addSubState m l	=	(SolverState 
	(context l) 
	(targetExps l) 
	(statementExps l) 
	(m:(subStates l)))
	
-- Creates a solver state given a context size, expression to solve for and
-- a true statement about the context.
	
initSolver	::	(Cons a) => Int -> Expression a -> Expression a -> SolverState a
initSolver c t s	=	SolverState c (Set.singleton t) (Set.singleton s) []

-- Continously processes a solver state until the callback returns true.

solve			::	(Cons a) => SolverState a -> (SolverState a -> IO Bool) -> IO (SolverState a)
solve s c	=	do
						continue	<-	c s
						(	if 	continue
							then	solve (process s) c
							else	return s)
	
-- Processes a solver state... by a little. This will need to be called multiple times
-- to get a result. However, this is guarenteed to not go into an infinite loop. That would
-- be bad.
	
process		::	(Cons a) => SolverState a -> SolverState a
process s	=	res
	where
		mainsolve	=	
			(Set.fold (\w x ->
				(Set.fold (\y z ->
						processRule (context s) w y z
					)
				) x (statementExps s)
			) s (targetExps s))
		res			=	mainsolve


-- Processes a single target statement pair of expressions in a solver state.

processRule	::	(Cons a) => Int -> Expression a -> Expression a -> SolverState a -> SolverState a

processRule c t s@(Function (Function (Constant l) m) n) state
	|	l == andCons	=	removeStatement s $ addStatement m $ addStatement n state
	|	l == equalCons	=	addTarget (substitute c t m n) $ addTarget (substitute c t n m) state
	
-- Given a context size, search expression, pattern and substitute, this will replace
-- all occurences of the pattern with the substitute.
	
substitute	::	(Cons a) => Int -> Expression a -> Expression a -> Expression a -> Expression a

substitute c x y z
	|	x == y			=	z
	|	otherwise		=	case x of
		(Function n m)	->	(Function (substitute c n y z) (substitute c m y z))
		x					->	x 