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
	mapVar,
	instantiate,
	match,
	Rule(..),
	Query(..),
	Solver(..),
	quickSolve
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
data Expression a b	=	
	Variable a												|
	Constant b												|
	Function (Expression a b) (Expression a b)	|
	Lambda (Expression a b) a							deriving (Show, Eq, Ord)
	
-- Describes a mapping of variables to expressions in an expression.
type Instance a b	=	Map a (Expression a b)
	
-- Maps the variables in an expression.
mapVar	::	(a -> b) -> Expression a c -> Expression b c
mapVar f (Variable v)	=	Variable (f v)
mapVar f (Constant c)	=	Constant c
mapVar f (Function l r)	=	Function (mapVar f l) (mapVar f r)
mapVar f (Lambda e v)	=	Lambda (mapVar f e) (f v)

-- Given a pattern expression and an instance, creates a concrete expression.
instantiate	::	(Ord a) => Expression a b -> Instance a b -> Expression a b
instantiate	(Variable v) f		=	f ! v
instantiate (Constant c) f		=	Constant c
instantiate (Function l r) f	=	Function (instantiate l f) (instantiate r f)
instantiate (Lambda e v) f		=	Lambda (instantiate e (Map.delete v f)) v

-- Creates an instance from a pattern expression and a concrete expression.
match		::	(Ord a, Eq b) => Expression a b -> Expression a b -> Maybe (Instance a b)
match (Variable v) e								=	Just $ singleton v e
match (Constant c) (Constant l)
	|	c  == l										=	Just $ Map.empty
	|	otherwise									=	Nothing
match (Constant c) _								=	Nothing
match (Function al ar) (Function bl br)	=	case (match al bl, match ar br) of
																(Just l, Just r)	->	case Map.fold (\it ac -> it && ac) True $ Map.intersectionWith (\a b -> a == b) l r of
																	(True)				->	Just (Map.union l r)
																	(False)				->	Nothing
																(_, _)				->	Nothing
match (Function _ _) _							=	Nothing
match (Lambda ae av) (Lambda be bv)			=	case (match ae be) of 
																(Just l)		->	case (Map.lookup av l) of
																	(Just m)		->	case m == Variable bv of
																		(True)		->	Just $ Map.delete av l
																		(False)		->	Nothing
																	(Nothing)	->	Just l
																Nothing		->	Nothing
															
																
-- Describes an equivalence of patterns (as an axiom).										
data Rule b			=	forall a. Ord a => Rule (Expression a b) (Expression a b)

-- Describes a task for a solver. A query requires the solver to find some expression in terms of a pattern
-- using a set of axioms.
data Query a b		=	Query (Expression a b) (Expression () b)

-- Solves a query using a number to indicate how many rules to evaluate.
type Solver a b	=	[Rule b] -> Query a b -> Integer -> [Instance a b]

-- Really long name for a solver.
generalPurposeRecursiveSolver	::	Solver a b -> Solver a b
generalPurposeRecursiveSolver solve axioms query max	=	res
	where
		

-- Tye teh knot!
quickSolve	=	generalPurposeRecursiveSolver quickSolve