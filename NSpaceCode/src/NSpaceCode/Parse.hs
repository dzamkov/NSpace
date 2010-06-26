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
import NSpaceCode.Value

-- Everything here works as long as you don't question it.

-- A description of a set of valid strings.

data Pattern	=
	Atom String |
	Concat Pattern Pattern | 
	Union Pattern Pattern |
	Repeat Pattern |
	AnyChar |
	StringExp |
	WordExp |
	Exp Operators deriving (Show, Eq, Ord)
	
-- Possible results given from a match.
	
data MatchResult	=
	AtomMatch |
	ConcatMatch MatchResult MatchResult |
	UnionMatch Pattern MatchResult |
	RepeatMatch [MatchResult] |
	AnyCharMatch Char |
	StringExpMatch String |
	WordExpMatch String |
	ExpMatch (Expression SimpleCons) (Map.Map String Int) Operator deriving (Show, Eq, Ord)
	-- ExpMatch provides expression, var map and the operator that binds it.
	
-- Operator information
	
data Associativity	=
	LeftAssociative |
	RightAssociative deriving (Show, Eq, Ord)
	
type Operators			=	[(Associativity, [String])]

data Operator	=
	Operator Operators String |
	StrongOperator deriving (Show, Eq, Ord)
	
-- Matches a pattern to a string and gives a result.
	
match	::	Pattern -> String -> (Set.Set MatchResult)

match (Atom x) y
	|	x == y		=	Set.singleton AtomMatch
	|	otherwise	=	Set.empty
	
	
match (Concat (Atom x) y) z
	|	length x <= length z		=	res
	|	otherwise					=	Set.empty
		where
			atomstr		=	take (length x) z
			otherstr		=	drop (length x) z
			atommatch	=	match (Atom x) atomstr
			othermatch	=	match y otherstr
			res			=	Set.fold (\l a -> 
									Set.fold (\m b -> 
										(Set.insert (ConcatMatch m l) b)) 
									a atommatch) 
								(Set.empty) othermatch
								
match (Concat x (Atom y)) z
	|	length y <= length z		=	res
	|	otherwise					=	Set.empty
		where
			atomstr		=	drop (length z - length y) z
			otherstr		=	take (length z - length y) z
			atommatch	=	match (Atom y) atomstr
			othermatch	=	match x otherstr
			res			=	Set.fold (\l a -> 
									Set.fold (\m b -> 
										(Set.insert (ConcatMatch l m) b)) 
									a atommatch) 
								(Set.empty) othermatch
			
	
match (Concat x y) z	=	res
	where
		matchOne :: Pattern -> Pattern -> String -> String -> Int -> Set.Set (MatchResult)
		matchOne x y st ns z
			|	length st == z			=	(Set.fold (\l a -> (
													Set.fold (\m b -> (
															Set.insert (ConcatMatch l m) b
														)) a (match y ns)
													)) (Set.empty) (match x st))
		matchOne x y st (n:ns) z	=	matchOne x y (st ++ [n]) ns z		
		res	=	foldr (\l a -> Set.union (matchOne x y [] z l) a) (Set.empty) [0..(length z)]
		
		
		
match (Union x y) z	= (Set.union
	(Set.fold (\l a -> Set.insert (UnionMatch y l) a) (Set.empty) (match y z))
	(Set.fold (\l a -> Set.insert (UnionMatch x l) a) (Set.empty) (match x z)))
	
	
	
match (Repeat x) z	=	res
	where
		mypat		= 	Union (Atom "") (Concat x (Repeat x))
		mymatch	=	match mypat z
		res		=	Set.map (\l -> case l of
							(UnionMatch _ (ConcatMatch j (RepeatMatch k)))	->	RepeatMatch (j:k)
							(UnionMatch _ _)											->	RepeatMatch []) mymatch
							
							
							
match (AnyChar) (z:[])	=	Set.singleton (AnyCharMatch z)
match (AnyChar) _			=	Set.empty

		
	
match (StringExp) z	=	res
	where
		unescape			::	Char -> Char
		unescape 'n'	=	'\n'
		unescape 't'	=	'\t'
		unescape 'r'	=	'\r'
		unescape	'"'	=	'"'
		unescape '\''	=	'\''
	
		quotechar	=	Union (Atom "\"") (Atom "'")
		instring		=	Repeat (Union AnyChar (Concat (Atom "\\") AnyChar))
		string		=	Concat quotechar (Concat instring quotechar)
		
		instringparse		::	String -> MatchResult -> Maybe String
		instringparse q (RepeatMatch r)	=	sequence (map (\l -> case l of
			(UnionMatch AnyChar (AnyCharMatch c))
				|	[c] == q												->	Nothing
				|	otherwise											->	Just c
			(UnionMatch _ (ConcatMatch _ (AnyCharMatch c)))	->	Just (unescape c)) r)
		
		stringmatch	=	match string z
		res			=	Set.map (\l -> case l of (Just m) -> m) $
				Set.filter (\l -> l /= Nothing) $
				Set.map (\l -> case l of
					(ConcatMatch (UnionMatch (Atom sqoute) _) (ConcatMatch i (UnionMatch (Atom eqoute) _)))
						|	sqoute == eqoute		->		(case (instringparse sqoute i) of
																	(Just s)	->	Just (StringExpMatch s)
																	Nothing	->	Nothing)
					_									->		Nothing) stringmatch

match (WordExp) z		=	res
	where
		wordchars	=	Set.fromList (['a'..'z'] ++ ['A' .. 'Z'] ++ "+-*&^%$#@!~<>|:/'")
		wordmatch	=	Repeat AnyChar
		charlist		=	case Set.findMax (match wordmatch z) of
								(RepeatMatch cl)	->	map (\l -> case l of (AnyCharMatch c) -> c) cl
		res			=	case (sequence $ map (\l -> 	if		Set.member l wordchars
																	then	Just l
																	else	Nothing) charlist) of
								(Just w)		->		Set.singleton $ WordExpMatch w
								Nothing		->		Set.empty
								
match (Exp ops) z	=	res
	where
	
		res	=	Set.union
						(Set.map (\l -> case l of
							(WordExpMatch w)	->	ExpMatch (Variable 0) (Map.singleton w 0) StrongOperator) (match WordExp z)) $
						(Set.map (\l -> case l of
							(ConcatMatch _ (ConcatMatch exp _))	->	exp) 
								(match (Concat (Atom "(") (Concat (Exp ops) (Atom ")"))) z))