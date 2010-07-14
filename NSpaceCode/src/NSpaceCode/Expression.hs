-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Literal(..),
	ModifierType(..),
	Expression(..),
	Pattern(..),
	Instance(..),
	Rule(..),
	boundVars,
	replaceVar,
	joinVars
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map

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

--	A type of modifier, an expression which describes a relation on a variable.
	
data ModifierType	=
	Forall	|
	Exists	|
	Lambda	|
	Solve		deriving(Show, Ord, Eq)
	
-- A relation between variables, and functions
-- that produces a definite value.

data Expression	=	
	Variable 															|
	Constant Literal													|
	Function Expression Expression (Set.Set (Int, Int))	|
	Modifier	ModifierType Expression Int						deriving (Show, Eq, Ord)
	
-- Gets the amount of bound variables in an expression.

boundVars	::	Expression -> Int
boundVars (Variable)			=	1
boundVars (Constant _)		=	0
boundVars (Function a b c)	=	(boundVars a) + (boundVars b) - (Set.size c)
boundVars (Modifier _ a _)	=	(boundVars a) - 1
		
-- Replaces a single indexed variable in an expression with another expression

replaceVar	::	Int -> Expression -> Expression -> Expression
replaceVar _ r (Variable)			=	r
replaceVar i r (Function a b m)	=	res
	where
		-- Finds the index of a variable in the "b" of a function
		bIndex	::	Int -> Set.Set (Int, Int) -> Int
		bIndex x y	=	sIndex x 0 y
			where
				sIndex t c y	=	case (Set.fold (\it ac -> case it of
											(_, b) -> 	if		b == c
															then	Just ()
															else	ac) Nothing y) of
											(Just _)		->		sIndex t (c + 1) y
											Nothing		->		if		t == 0
																	then	c
																	else	sIndex (t - 1) (c + 1) y
	
		asize			=	boundVars a
		rsize			=	boundVars r
		replaceInA	=	res
			where
				joint		=	Set.fold (\it ac -> case it of
									(a, b)
										|	a == i		->	Just b
										|	otherwise	->	ac
								) Nothing m
								
				replaceInBoth	=	res
					where
						bi			=	case joint of (Just x) -> x
						nm			=	Set.fold (\it ac -> case it of
											(a, b)
												|	a > i && b > i				->	Set.insert (a + rsize - 1, b + rsize - 1) ac
												|	a > i							->	Set.insert (a + rsize - 1, b) ac
												|	b > i							->	Set.insert (a, b + rsize - 1) ac
												|	a == i && b == bi			->	Set.union (Set.fromList [(x + i, x + bi) | x <- [0..(rsize - 1)]]) ac
												|	otherwise					->	Set.insert it ac
										) Set.empty m
						res		=	Function (replaceVar i r a) (replaceVar bi r b) nm
						
								
				res		=	case joint of
									(Nothing)	->	Function (replaceVar i r a) b (Set.fold 
															(\it ac -> case it of
																(a, b)
																	|	a > i			->	Set.insert (a + rsize - 1, b) ac
																	|	otherwise	->	Set.insert it ac
														) Set.empty m)
									(Just x)		->	replaceInBoth
				
		replaceInB	=	res
			where
				bindex	=	(bIndex (i - asize) m)
				nm			=	Set.fold (\it ac -> case it of
									(a, b)
										|	b > bindex		->	Set.insert (a, b + rsize - 1) ac
										|	otherwise		->	Set.insert it ac
								) Set.empty m
				res		=	Function a (replaceVar bindex r b) nm
		
		res	=	if		i < asize
					then	replaceInA
					else	replaceInB
replaceVar i r (Modifier t e a)
	|	i < a								=	Modifier t (replaceVar i r e) (a + (boundVars r) - 1)
	|	otherwise						=	Modifier t (replaceVar (i + 1) r e) a
		
-- Sets two indexed variables to equal the same. The last specified variable is removed.
		
joinVars		=	error "Not Implemented Yet"		
		
		
-- A pattern is an expression with some terms missing, being instead replaced
-- by a free variable.
type Pattern = Expression

-- Information about what "fits" into the free variables of a pattern. A pattern
-- combined with an instance can be used to generate an expression.

type Instance = Map.Map Int Expression
		
-- A rule describes a possible equivalence between values. It has an option condition (set to true
--	if rule is unconditional) and two corresponding patterns known to be equivalent if the condition
-- is met.
		
data Rule		=	Rule (Pattern) (Pattern) (Pattern) deriving(Show)