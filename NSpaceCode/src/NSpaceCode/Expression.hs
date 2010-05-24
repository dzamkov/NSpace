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
	createFunc,
	createForAll,
	replaceVar,
	matchExp,
	SpecialVarSet(..),
	TruthSet(..),
	createProgram
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map

-- A relation between variables, and functions
-- that produces a definite value. The provided type to
-- Expression determines the type of the value it can 
-- evaluate to.

data Expression		=	
	Variable Int |
	Function Expression Expression |
	ForAll (Set.Set Int) Expression deriving (Show, Eq)
	
-- Creates a new variable from a variable index.
	
createVar		:: Int -> Expression
createVar x		=	Variable x

-- Creates a function as the application of an expression on
-- another. There should be a direct relation of variables between
-- the two expression.

createFunc			::	Expression -> Expression -> Expression
createFunc x y		=	Function x y

-- Creates a special forall expression which is true if and only if the
-- inner expression is true for all possible values of the specified variables.
											
createForAll			:: (Set.Set Int) -> Expression -> Expression
createForAll x y		=	ForAll x y
												
-- Programs are defined by a special variable set and a true expression. A truth
-- set contains expressions that are all true.
							
data SpecialVarSet	=	SpecialVarSet {
	getTrueVar		::	Int,
	getFalseVar		::	Int,
	getEqualsVar	::	Int,
	getAndVar		::	Int,
	getIFTVar		::	Int } deriving (Show, Eq)
	
data TruthSet	=	TruthSet {
	getTrueExpressions	::	Set.Set Expression,
	getSpecialVarSet	::	SpecialVarSet }
	
-- Creates a program from a true expression and special var set.
	
createProgram		::	Expression -> SpecialVarSet -> TruthSet
createProgram x y	=	TruthSet (Set.singleton x) y

-- Replaces a variable in an expression.

replaceVar	::	Expression -> Int -> Int -> Expression
replaceVar (Variable a) b c		=	if	a == b
												then (Variable c)
												else (Variable a)
replaceVar (Function a b) c d		=	Function (replaceVar a c d) (replaceVar b c d)
replaceVar (ForAll a b) c d		=	if Set.member c a
												then ForAll a b
												else ForAll a (replaceVar b c d)

-- Given a target expression, pattern expression and set of free variables
-- in the pattern expression, this will determine if the target and pattern
-- expression "match" and the values of the free variables if so.

matchExp	::	Expression -> Expression -> Set.Set Int -> Maybe (Map.Map Int Expression)
matchExp a (Variable b) c 			= 	if Set.member b c
												then Just (Map.singleton b a)
												else	case a of
															(Variable x) 	-> 	if x == b
																						then Just (Map.empty)
																						else Nothing
															_					->		Nothing
matchExp (Function a b) (Function c d) e		=	case ((matchExp a c e), (matchExp b d e)) of
																	((Just x), (Just y))		->		Just (Map.union x y)
																	_								-> 	Nothing
matchExp (ForAll a b) (ForAll c d) e		=	matchExp testexp d e
														where
															substitution	=	zip (Set.toList a) (Set.toList c)	::	[(Int, Int)]
															testexp			=	foldr (\x y -> replaceVar y (fst x) (snd x)) b substitution		:: Expression