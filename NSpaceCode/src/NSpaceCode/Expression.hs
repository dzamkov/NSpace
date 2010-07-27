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
	correspond,
	patternMatch,
	Rule(..),
	Query(..),
	QueryResult(..),
	computeResult,
	lambdaAxioms
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
								
-- Finds subexpressions in the second expression that correspond to terms in
-- the first part.
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
correspond _ _												=	Nothing

-- Creates an instance from a pattern expression and a concrete expression.
match		:: (Ord a) => Pattern a -> Expression a -> Maybe (Instance a)
match p e	=	res
	where
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
				
-- Matches one pattern against another.				
patternMatch	::	(Ord a) => Pattern a -> Pattern a -> Maybe (Instance (Either Int a))
patternMatch p e	=	res
	where
		res		=	case (correspond p e) of 
							(Just m) ->	foldl (\ac it -> case (ac, it) of
								(Just a, (Left x, y))					->	case Map.lookup x a of
									(Just z)										->	case z == y of
										(True)										->	Just a
										(False)										->	Nothing
									Nothing										->	Just (Map.insert x y a)
								(Just a, (Right x, Term (Right y)))	->	case x == y of
									(True)										->	Just a
									(False)										->	Nothing
								_												->	Nothing
											) (Just (Map.empty)) m
							Nothing	->	Nothing
																
-- Describes an equivalence of patterns (as an axiom).										
data Rule a			=	Rule (Pattern a) (Pattern a)

-- Describes a task for a solver. A query requires the solver to find some expression in terms of a pattern
-- using a set of axioms.
data Query a		=	Query (Pattern a) (Expression a)

-- A result given for a query. Such a result includes all sub queries to be completed (they may be partially
-- computed) and gives a function which takes the processed queries and computes the instances for the main query.
data QueryResult a	=	forall b. Ord b => QueryResult [Query b] ([[Instance b]] -> [Instance a])

computeResult	:: (Ord a) => [Rule a] -> Query a -> QueryResult a
computeResult rules qry@(Query tarpat tarexp)	=	res
	where
	
		-- Creates a function which can create instances of the main query by getting
		-- the results of sub queries.
		ruleGen	::	(Ord a) => Query a -> Pattern a -> Pattern a -> Maybe (Query a, Instance a -> Instance a)
		ruleGen (Query tarpat tarexp) con tar	=	res
			where
				tarmatch	=	patternMatch tarpat tar
				res		=	case tarmatch of
									(Just i)	-> Just (Query con tarexp, (\inst -> Map.map (\instvar -> instantiate instvar inst) i))
									Nothing	->	Nothing
									
		ruleFuncs	=	foldl (\ac it -> case (it) of
								(Rule a b)				->	case (ruleGen qry a b, ruleGen qry b a) of
									(Just l, Just m)		->	[l, m] ++ ac
									(Just l, Nothing)		->	[l] ++ ac
									(Nothing, Just m)		->	[m] ++ ac
									(Nothing, Nothing)	->	ac
							) [] rules
		ruleQrs		=	map (\l -> fst l) ruleFuncs
		ruleTrs		=	map (\l -> snd l) ruleFuncs
		ruleFn		=	(\insts	->
								foldl (\ac it -> case it of
									(qreses, func)	->	foldl (\ac it -> (func it):ac) ac qreses
								) [] (zip insts ruleTrs)
							)
	
		res	=	case (tarpat, tarexp) of
						_		->	QueryResult ruleQrs ruleFn
				
-- Rules true for all systems allowing lambda terms.				
lambdaAxioms	::	[Rule a]
lambdaAxioms	=	[
		(Rule
			(Function (Lambda (Term Nothing)) (Term $ Left 0))
			(Term $ Left 0)
		), (Rule
			(Function (Lambda (Function (Term $ Just $ Left 1) (Term Nothing))) (Term $ Left 0))
			(Function (Term $ Left 1) (Term $ Left 0))
		)
	]