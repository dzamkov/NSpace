-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Parse
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Parse (
	Pattern(..),
	MatchResult(..),
	match
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression

-- A description of a set of valid strings.

data Pattern	=
	Atom String |
	Concat Pattern Pattern deriving (Show, Eq, Ord)
	
-- Possible results given from a match.
	
data MatchResult	=
	NoMatch |
	AtomMatch |
	ConcatMatch (Set.Set (MatchResult, MatchResult)) deriving (Show, Eq, Ord)
	
-- Matches a pattern to a string and gives a result.
	
match	::	Pattern -> String -> MatchResult

match (Atom x) y
	|	x == y		=	AtomMatch
	|	otherwise	=	NoMatch
	
match (Concat (Atom x) y) z	=	case (match (Atom x) (take (length x) z), match y (drop (length x) z)) of
												(NoMatch, _)	->	NoMatch
												(_, NoMatch)	->	NoMatch
												(x, y)			->	ConcatMatch (Set.singleton (x, y))
												
match (Concat x (Atom y)) z	=	case (match x (take (length z - length y) z), match (Atom y) (drop (length z - length y) z)) of
												(NoMatch, _)	->	NoMatch
												(_, NoMatch)	->	NoMatch
												(x, y)			->	ConcatMatch (Set.singleton (x, y))
	
match (Concat x y) z	=	res
	where
		matchOne :: Pattern -> Pattern -> String -> String -> Int -> Maybe (MatchResult, MatchResult)
		matchOne x y st ns z
			|	length st == z			=	case (match x st, match y ns) of
													(NoMatch, _)	->	Nothing
													(_, NoMatch)	->	Nothing
													(x, y)			->	Just (x, y)
		matchOne x y st (n:ns) z	=	matchOne x y (st ++ [n]) ns z		
		res	=	ConcatMatch $ foldr (\l a -> case (matchOne x y [] z l) of
								Just m	->	Set.insert m a
								Nothing	->	a) (Set.empty) [0..(length z)]
								
