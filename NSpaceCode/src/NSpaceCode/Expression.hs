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
	ForAll Int (Expression a) deriving (Show, Eq, Ord)
	
-- Expression that is ordered by its simplicity, with simplest expression greater
-- than complex one. OCCAMS RAZOR FTW!
	
newtype ScoredExpression a =	ScoredExpression (Expression a) deriving (Show, Eq)

instance (Cons a) => Ord (ScoredExpression a) where
	compare (ScoredExpression x) (ScoredExpression y)		=	if		scorecomp == EQ
																				then	compare x y
																				else	scorecomp
		where
			-- This is an approximation, don't yell at me
			score (Variable _)	=	1
			score (Constant _)	=	1
			score (Function m n)	=	score m + score n
			score (ForAll _ m)	=	(score m) * 2
			
			scorecomp	=	compare (score y) (score x)
	
-- State of a solver at any give time.

data SolverState a	=	SolverState {
		openTargetExps		::	(Set.Set (ScoredExpression a)),
		closedTargetExps	::	(Set.Set (Expression a)),
		statementExps		::	(Set.Set (Expression a)),
		subStates			::	[SolverState a]
	} deriving (Show)
	
addTarget			::	(Cons a) => Expression a -> SolverState a -> SolverState a
addTarget m l	=	if 	(Set.member m (closedTargetExps l))
						then	l
						else	(SolverState 
	(Set.insert (ScoredExpression m) (openTargetExps l))
	(closedTargetExps l)
	(statementExps l)
	(subStates l))
	
closeTarget			::	(Cons a) => Expression a -> SolverState a -> SolverState a
closeTarget m l	=	(SolverState 
	(Set.delete (ScoredExpression m) (openTargetExps l))
	(Set.insert m (closedTargetExps l))
	(statementExps l)
	(subStates l))

addStatement		::	(Cons a) => Expression a -> SolverState a -> SolverState a
addStatement m l	=	(SolverState 
	(Set.union (openTargetExps l) (Set.map (\x -> ScoredExpression x) (closedTargetExps l))) 
	(Set.empty)
	(Set.insert m (statementExps l)) 
	(subStates l))

removeStatement	::	(Cons a) => Expression a -> SolverState a -> SolverState a
removeStatement m l	=	(SolverState 
	(openTargetExps l)
	(closedTargetExps l)
	(Set.delete m (statementExps l)) 
	(subStates l))
	
addSubState			::	(Cons a) => SolverState a -> SolverState a -> SolverState a
addSubState m l	=	(SolverState 
	(openTargetExps l)
	(closedTargetExps l)
	(statementExps l) 
	(m:(subStates l)))
	
-- Creates a solver state given a context size, expression to solve for and
-- a true statement about the context.
	
initSolver	::	(Cons a) => Expression a -> Expression a -> SolverState a
initSolver t s	=	SolverState (Set.singleton (ScoredExpression t)) (Set.empty) (Set.singleton s) []

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
		topexp		=	case Set.findMax (openTargetExps s) of (ScoredExpression x) -> x
		mainsolve	= (Set.fold (\y z -> processRule topexp y z) 
				(closeTarget topexp s) (statementExps s))
		res			=	mainsolve


-- Processes a single target statement pair of expressions in a solver state.

processRule	::	(Cons a) =>	Expression a -> Expression a -> SolverState a -> SolverState a

processRule t s@(Function (Function (Constant l) m) n) state
	|	l == andCons	=	removeStatement s $ addStatement m $ addStatement n state
	|	l == equalCons	=	addTarget (substitute t m n) $ addTarget (substitute t n m) state
	
-- Given a context size, search expression, pattern and substitute, this will replace
-- all occurences of the pattern with the substitute.
	
substitute	::	(Cons a) => Expression a -> Expression a -> Expression a -> Expression a

substitute x y z
	|	x == y			=	z
	|	otherwise		=	case x of
		(Function n m)	->	(Function (substitute n y z) (substitute m y z))
		x					->	x 