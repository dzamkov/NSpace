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
	Query(..),
	boundVars,
	replaceVar,
	reduce,
	compute
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

data Expression a	=	
	Variable a										|
	Constant Literal								|
	Function Expression Expression			|
	Modifier	ModifierType Expression a		deriving (Show, Eq, Ord)

-- A pattern is an expression with some terms missing, being instead replaced
-- by a free variable.
type Pattern a		=	Expression (Either Int a)

-- Information about what "fits" into the free variables of a pattern. A pattern
-- combined with an instance can be used to generate an expression.

type Instance a	=	Map.Map Int (Expression a)
		
-- A rule describes a possible equivalence between values. It has an option condition (set to true
--	if rule is unconditional) and two corresponding patterns known to be equivalent if the condition
-- is met. It also contains a mapping of variables in the three patterns.
		
data Rule a		=	Rule 
							(Pattern a) 
							(Pattern a) 
							(Pattern a) deriving(Show, Eq, Ord)
						
--	Represents a reduction query, which describes the take of converting an expression in one
-- form to another specified by a pattern using a set of rules.

data Query a	=	Query (Pattern a) (Expression a)

-- Part of a query obtained from applying a single rule to a subquery.

data IntermediateQuery a	=	IntermediateQuery
											(Query a)
											(Pattern a)
											(Pattern a)

-- An intermediate result of a query

data QueryResult a			=	QueryResult (Set.Set (Instance a)) [IntermediateQuery a] 


-- Tries to fill a query with an instance with a set of rules.
compute	::	[Rule a] -> Query a -> Set.Set (Instance a)