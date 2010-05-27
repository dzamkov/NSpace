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
	Pattern(..),
	PatternReference(..),
	patternMatch,
	toPattern,
	createVar,
	createFunc,
	createForAll,
	createScope,
	getBoundVars,
	getDynamicVars,
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
	Scope (Set.Set a) (Expression a) deriving (Show, Ord, Eq)
	
-- A pattern of an expression that can match a variety of expressions based
-- on a template expression.
	
type Pattern a b		=	Expression (PatternReference a b)
	
-- Data that uniquely identifies a particular variable.
	
class (Eq a, Ord a) => Reference a where
	equalRef		::	a	-- Reference to the equal function ((eq a) b) = (a = b)
	andRef		:: a	-- Reference to the and function
	orRef			::	a	-- Reference to the or function
	iteRef		::	a	-- Reference to the if then else function
	notRef		::	a	-- Reference to the not function
	trueRef		::	a	-- Reference to true constant
	falseRef		::	a	-- Reference to false constant
	
-- Reference type used in patterns. A definite reference matches only the
-- specified reference. A flexible reference matches any reference.

data PatternReference a b		=	
	Definite a |
	Flexible b deriving (Show, Ord, Eq)
	
instance (Reference a, Ord a, Ord b, Eq b) => Reference (PatternReference a b) where
	equalRef		=	(Definite equalRef)
 	andRef		=	(Definite andRef)
	orRef			=	(Definite orRef)
	iteRef		=	(Definite iteRef)
	notRef		=	(Definite notRef)
	trueRef		=	(Definite trueRef)
	falseRef		=	(Definite falseRef)
	
-- Converts an expression to a pattern.

toPattern						:: (Ord a, Ord b) => Expression a -> Pattern a b
toPattern (Variable x)		=	Variable (Definite x)
toPattern (Function x y)	=	Function (toPattern x) (toPattern y)
toPattern (ForAll x y)		=	ForAll (Set.map (\z -> Definite z) x) (toPattern y)
toPattern (Scope x y)		=	Scope (Set.map (\z -> Definite z) x) (toPattern y) 
	
-- Matches a pattern to an expression and fills in the flexible values
-- if the match was sucsessful.
	
patternMatch	::	(Ord b) => Pattern a b -> Expression a -> Maybe (Map.Map b (Expression a))

patternMatch (Variable (Flexible x)) y		=	Just (Map.singleton x y)	

patternMatch (Function i h) (Function j k)	=	case (patternMatch i j, patternMatch h k) of
		(Just x, Just y)		->		Just (Map.union x y)
		_							->		Nothing
		
patternMatch _ _	=	Nothing

-- Creates a new variable expression.
	
createVar		:: a -> Expression a
createVar x		=	Variable x

-- Gets a reference for a variable.

getRef					::	Expression a -> a
getRef (Variable x)	=	x
getRef _					=	undefined

-- Creates a function as the application of an expression on
-- another. There should be a direct relation of variables between
-- the two expression.

createFunc			::	Expression a -> Expression a -> Expression a
createFunc x y		=	Function x y

-- Creates a special forall expression which is true if and only if the
-- inner expression is true for all possible values of the specified variables.
											
createForAll			:: (Ord a) => (Set.Set a) -> Expression a -> Expression a
createForAll x y		=	ForAll x y

-- Creates a scope, which hides the specified "local" variables.

createScope			::	(Set.Set a) -> Expression a -> Expression a
createScope x y	=	Scope x y
												
-- Gets the set of variables bound by an expression.
												
getBoundVars						::	(Ord a) => Expression a -> (Set.Set a)
getBoundVars (Variable x)		=	Set.singleton x
getBoundVars (Function x y)	=	Set.union (getBoundVars x) (getBoundVars y)
getBoundVars (ForAll x y)		=	(getBoundVars y)
getBoundVars (Scope x y)		=	Set.difference (getBoundVars y) x

-- Given a forall expression, this will return all the dynamic
-- variables defined by it.

getDynamicVars							::	(Ord a) => Expression a -> (Set.Set a) 
getDynamicVars (ForAll x _)		=	x
getDynamicVars _						=	Set.empty