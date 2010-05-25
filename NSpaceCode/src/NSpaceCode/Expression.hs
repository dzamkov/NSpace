-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression,
	Reference(..),
	createVar,
	createFunc,
	createForAll,
	getBoundVars,
	getDynamicVars,
	replaceVar,
	matchExp,
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map

-- A relation between variables, and functions
-- that produces a definite value. The provided type to
-- Expression determines the type of the value it can 
-- evaluate to.

data Expression a		=	
	Variable a |
	Function (Expression a) (Expression a) |
	ForAll (Set.Set a) (Expression a) |
	Scope (Map.Map a a) (Expression a) deriving (Show, Eq)
	
-- Data that uniquely identifies a particular variable.
	
class (Eq a) => Reference a where
	equalRef		::	a	-- Reference to the equal function ((eq a) b) = (a = b)
	andRef		:: a	-- Reference to the and function
	orRef			::	a	-- Reference to the or function
	iteRef		::	a	-- Reference to the if then else function
	notRef		::	a	-- Reference to the not function
	
-- Creates a new variable expression.
	
createVar		:: a -> Expression a
createVar x		=	Variable x

-- Creates a function as the application of an expression on
-- another. There should be a direct relation of variables between
-- the two expression.

createFunc			::	Expression a -> Expression a -> Expression a
createFunc x y		=	Function x y

-- Creates a special forall expression which is true if and only if the
-- inner expression is true for all possible values of the specified variables.
											
createForAll			:: (Ord a) => (Set.Set a) -> Expression a -> Expression a
createForAll x y		=	ForAll x y
												
-- Gets the set of variables bound by an expression.
												
getBoundVars						::	(Ord a) => Expression a -> (Set.Set a)
getBoundVars (Variable x)		=	Set.singleton x
getBoundVars (Function x y)	=	Set.union (getBoundVars x) (getBoundVars y)
getBoundVars (ForAll x y)		=	(getBoundVars y)
getBoundVars (Scope x y)		=	Map.foldrWithKey (\k a b -> if Set.member a innerbound then Set.insert k b else b) (Set.empty) x
										where
											innerbound = (getBoundVars y)

-- Given a forall expression, this will return all the dynamic
-- variables defined by it.

getDynamicVars							::	(Ord a) => Expression a -> (Set.Set a) 
getDynamicVars (ForAll x _)		=	x
getDynamicVars _						=	Set.empty
												
-- Replaces a variable in an expression.

replaceVar	::	(Eq a) => Expression a -> a -> a -> Expression a
replaceVar (Variable a) b c		=	if	a == b
												then (Variable c)
												else (Variable a)
replaceVar (Function a b) c d		=	Function (replaceVar a c d) (replaceVar b c d)

-- Given a target expression, pattern expression and set of free variables
-- in the pattern expression, this will determine if the target and pattern
-- expression "match" and the values of the free variables if so.

matchExp	::	(Ord a) => Expression a -> Expression a -> Set.Set a -> Maybe (Map.Map a (Expression a))
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