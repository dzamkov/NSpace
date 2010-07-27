-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Literal(..),
	Expression(..),
	Instance(..),
	exprMap,
	instantiate,
	match,
	Rule(..),
	Query(..)
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map, (!), singleton, insert)
import List

-- Logic and numerical Ltant
data Literal	=
	IntegerL (Integer)	|
	LogicL (Bool) 			|
	CharL (Char) 			|
	ListL 					|
	SetL 						|
	UniversalL 				|
	EqualL 					|
	PlusL 					|
	MinusL 					|
	TimesL 					|
	AndL 						|
	OrL 						|
	XorL 						|
	XandL 					|
	ITEL 						|
	NotL 						deriving (Show, Ord, Eq)
	
-- A relation between variables, and functions
-- that produces a definite value.
data Expression a		=	
	Term a												|
	Function (Expression a) (Expression a)		|
	Lambda (Expression (Maybe a))					deriving (Show, Eq, Ord)
	
-- A pattern or template for an expression.
type Pattern a			=	Expression (Either Int a)
	
-- Describes a mapping of variables to expressions in a pattern.
type Instance a		=	Map Int (Expression a)

-- Maps the terms in an expression.
exprMap	::	(a -> Expression b) -> Expression a -> Expression b
exprMap f (Term x)			=	(f x)
exprMap f (Function l r)	=	Function (exprMap f l) (exprMap f r)
exprMap f (Lambda e)			=	Lambda $ exprMap (\l -> case l of
											(Nothing)	->	Term $ Nothing
											(Just x)		->	exprMap (\m -> Term $ Just m) (f x)) e
											
exprMaybeMap	::	(a -> Maybe (Expression b)) -> Expression a -> Maybe (Expression b)
exprMaybeMap f (Term x)			=	(f x)
exprMaybeMap f (Function l r)	=	case (exprMaybeMap f l, exprMaybeMap f r) of
												(Just ll, Just rr)		->	Just (Function ll rr)
												_								->	Nothing
exprMaybeMap f (Lambda e)		=	case exprMaybeMap (\l -> case l of
												(Nothing)	->	Just (Term Nothing)
												(Just x)		->	case f x of
													(Just y)		->	Just $ exprMap (\l -> Term $ Just l) y
													Nothing		->	Nothing) e of
												(Just ne)	->	Just $ Lambda ne
												(Nothing)	->	Nothing

-- Given a pattern expression and an instance, creates a concrete expression.
instantiate	:: Pattern a -> Instance a -> Expression a
instantiate p f	=	exprMap (\l -> case l of
								(Left x)		->	f ! x
								(Right x)	->	Term x) p

-- Creates an instance from a pattern expression and a concrete expression.
match		:: (Ord a) => Pattern a -> Expression a -> Maybe (Instance a)
match p e	=	res
	where
		correspond :: Expression a -> Expression b -> Maybe [(a, Expression b)]
		correspond (Term x) e									=	Just [(x, e)]
		correspond (Function al ar) (Function bl br)		=	case (correspond al bl, correspond ar br) of
																				(Just am, Just bm) 	->	Just (am ++ bm)
																				_							->	Nothing
		correspond (Lambda a) (Lambda b)						=	case (correspond a b) of
																				(Just m)					->	foldl (\ac it -> case (ac, it) of
																					(Just x, (Nothing, Term Nothing))	->	Just x
																					(Just x, (Just y, z))					->	case (exprMaybeMap (\l -> case l of
																						(Just m)		->	Just (Term m)
																						(Nothing)	->	Nothing
																																		) z) of 
																						(Just nz)									->	Just ((y, nz):x)
																						(Nothing)									->	Nothing
																					_												->	Nothing
																												) (Just []) m
																				Nothing					->	Nothing
		_																=	Nothing
																				
		res		=	case (correspond p e) of
							(Just m)	->	foldl (\ac it -> case (ac, it) of
								(Just a, (Left x, y))			->	case Map.lookup x a of
									(Just z)								->	case z == y of
										(True)								->	Just a
										(False)								->	Nothing
									Nothing								->	Just (Map.insert x y a)
								(Just a, (Right x, y))			->	case (Term x) == y of
									(True)								->	Just a
									(False)								->	Nothing
								_										->	Nothing
											) (Just (Map.empty)) m
							Nothing	->	Nothing
																
-- Describes an equivalence of patterns (as an axiom).										
data Rule a			=	Rule (Pattern a) (Pattern a)

-- Describes a task for a solver. A query requires the solver to find some expression in terms of a pattern
-- using a set of axioms.
data Query a		=	Query (Pattern a) (Expression a)

-- A result given for a query. Such a result includes all sub queries to be completed (they may be partially
-- computed) and gives a function which takes the processed queries and computes the instances for the main query.
data QueryResult a	=	QueryResult [Query a] ([[Instance a]] -> [Instance a])

computeResult	:: Rule a -> Query a -> QueryResult a
computeResult rules query	=	res
	where
		res	=	error "not done yet"