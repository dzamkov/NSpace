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
	Associativity(..),
	Operators(..),
	Binding(..),
	defaultOperators,
	operatorInfo,
	isOperator,
	match
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression
import NSpaceCode.Value

-- Everything here works as long as you don't question it.

-- A description of a set of valid strings.

data Pattern	=
	Atom String |					-- Matches single string
	Concat Pattern Pattern | 	-- Matches both patterns together
	Union Pattern Pattern |		--	Matches either pattern
	Repeat Pattern |				--	Matches any amount of the specified pattern
	Possible Pattern |			-- Matches "" or the specified pattern
	AnyChar |						--	Matches any one character
	WhiteSpace |					--	Matches one or more whitespace char
	StringExp |						--	Matches program string
	WordExp |						--	Matches program word
	Exp Operators deriving (Show, Eq, Ord)
	
-- Possible results given from a match.
	
data MatchResult	=
	AtomMatch |
	ConcatMatch MatchResult MatchResult |
	UnionMatch Pattern MatchResult |
	PossibleMatch (Maybe MatchResult) |
	RepeatMatch [MatchResult] |
	AnyCharMatch Char |
	WhiteSpaceMatch |
	StringExpMatch String |
	WordExpMatch String |
	ExpMatch (Expression SimpleCons) (Map.Map String Int) Binding deriving (Show, Eq, Ord)
	-- ExpMatch provides expression, var map and the operator that binds it.
	
-- Operator information
	
data Associativity	=
	LeftAssociative |
	RightAssociative deriving (Show, Eq, Ord)
	
type Operators			=	[(Associativity, Set.Set String)]

defaultOperators		=	[
	(LeftAssociative, 	Set.fromList ["*"]),
	(LeftAssociative, 	Set.fromList ["+", "-"]),
	(LeftAssociative, 	Set.fromList [".."]),
	(LeftAssociative,		Set.fromList ["="]),
	(LeftAssociative,		Set.fromList ["or", "xor"]),
	(LeftAssociative,		Set.fromList ["and", "xand"])]
	
isOperator ::	String -> Operators -> Bool
isOperator op ops	=	or $ map (\l -> case l of
								(_, m)	->	Set.member op m) ops

operatorInfo	::	String -> Operators -> Maybe (Int, Associativity)
operatorInfo s ops	=	snd (foldl (\l m -> case (l, m) of
									((str, Nothing), (asc, oplist))
										|	Set.member s oplist	->	(str, Just (str, asc))
										|	otherwise				->	(str + 1, Nothing)
									(state, _)						->	state) (0, Nothing) ops)
								
data Binding	=	--	Identifies a binding type in the context of an operator set
	OperatorBinding Int Associativity |
	WordBinding |
	FunctionBinding |
	BracketBinding deriving (Show, Eq, Ord)
	
-- Gets if it is possible to create a function out of two adjacent terms of
-- the specified bindings.
canFunctionBind	::	Binding -> Binding -> Bool

canFunctionBind _ (OperatorBinding _ _)	=	False
canFunctionBind (OperatorBinding _ _) _	=	False
canFunctionBind _ FunctionBinding			=	False
canFunctionBind _ _								=	True

canOperatorBind	::	Binding -> (Int, Associativity) -> Binding -> Bool

canOperatorBind (OperatorBinding ls la) (ms, ma) (OperatorBinding rs ra)
	|	ms < ls && ms < rs	=	False
	|	ms > ls && ms > rs	=	True
	
canOperatorBind (OperatorBinding ls la) (ms, ma) _
	|	ms > ls										=	True
	|	ms == ls && ma == LeftAssociative	=	True
	|	otherwise									=	False
	
canOperatorBind _ (ms, ma) (OperatorBinding rs ra)
	|	ms > rs										=	True
	|	ms == rs && ma == RightAssociative	=	True
	|	otherwise									=	False
	
canOperatorBind _ _ _		=	True

-- Operator binding 		-	binding created by an operator
-- Word binding			-	default binding to a single word
-- Bracket binding		-	binding caused by ( ), strongest
	


maybeMap	::	(Ord a, Ord b) => (a -> Maybe b) -> Set.Set a -> Set.Set b
maybeMap y x	=	Set.map (\l -> case l of (Just m) -> m) $ 
						Set.filter (\l -> case l of 
							(Just _) -> True
							Nothing	->	False) $ 
						Set.map y x
	
	
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
								
match (Concat x (Concat WhiteSpace y)) z	=	res
	where
		whiteSpace			=	Set.fromList [' ', '\t', '\n']
		groupWhiteSpace	::	String -> [(Int, Int)]
		groupWhiteSpace s	=	case (foldl foldfunc (0, Nothing, []) s) of 
										(m, Just s, l) -> (s, m):l
										(_, Nothing, l) -> l
			where
				foldfunc	::	(Int, Maybe Int, [(Int, Int)]) -> Char -> (Int, Maybe Int, [(Int, Int)])
				foldfunc (cur, Nothing, grps) c
					|	Set.member c whiteSpace	=	(cur + 1, Just cur, grps)
					|	otherwise					=	(cur + 1, Nothing, grps)
				foldfunc (cur, Just start, grps) c
					|	Set.member c whiteSpace	=	(cur + 1, Just start, grps)
					|	otherwise					=	(cur + 1, Nothing, (start, cur):grps)
		
		res	=	foldl (\cur grp -> case grp of
						(start, end) -> 
							foldl (\cur pivs ->
								foldl (\cur pive ->
									Set.fold (\l cur ->
										Set.fold (\m cur ->
											Set.insert (ConcatMatch l (ConcatMatch WhiteSpaceMatch m)) cur
										) cur (match y (drop pive z))
									) cur (match x (take pivs z))
								) cur [(pivs + 1)..end]
							) cur [start..(end - 1)]
					) Set.empty (groupWhiteSpace z)
			
	
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
	
	
match (Possible x) z	=	Set.union
	(Set.map (\l -> PossibleMatch (Just l)) (match x z)) $
	(	if		z == ""
		then	Set.singleton (PossibleMatch Nothing)
		else	Set.empty)
	
	
match (Repeat x) z	=	res
	where
		mypat		= 	Union (Atom "") (Concat x (Repeat x))
		mymatch	=	match mypat z
		res		=	Set.map (\l -> case l of
							(UnionMatch _ (ConcatMatch j (RepeatMatch k)))	->	RepeatMatch (j:k)
							(UnionMatch _ _)											->	RepeatMatch []) mymatch
							
							
							
match (AnyChar) (z:[])	=	Set.singleton (AnyCharMatch z)
match (AnyChar) _			=	Set.empty

		
match (WhiteSpace) z		=	Set.map (\l -> WhiteSpaceMatch) $
	Set.filter (\l -> case l of (RepeatMatch m) -> length m > 0) $
	match (Repeat $ Union (Atom " ") $ Union (Atom "\t") (Atom "\n")) z
		
	
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
		startbracket	=	(Concat (Atom "(") (Possible WhiteSpace))
		endbracket		=	(Concat (Possible WhiteSpace) (Atom ")"))
		
		funcCombine			::	MatchResult -> MatchResult -> Maybe MatchResult	
		funcCombine (ExpMatch fe fm fb) (ExpMatch ae am ab)
			|	canFunctionBind fb ab	=	Just res
			|	otherwise					=	Nothing
				where
						intsect			=	Map.fromList $ Map.elems $ Map.intersectionWith (\l m -> (m, l)) fm am
						startunbound	=	(Set.findMax (getBound fe)) + 1
						newexp			=	Function fe $ rebind ae (\l ->	case Map.lookup l intsect of
													(Just m) -> m
													Nothing	-> l + startunbound)
						newmap			=	Map.unionWith (\l m -> l) fm $ (Map.map (\l -> l + startunbound) am)
						res				=	ExpMatch newexp newmap FunctionBinding
	
		wordexps			=	(maybeMap (\l -> case l of
									(WordExpMatch w)	
										|	isOperator w ops	-> Nothing
										|	otherwise			->	Just $ ExpMatch (Variable 0) (Map.singleton w 0) WordBinding) 
								(match WordExp z))
		
		bracketexps		=	(Set.map (\l -> case l of
									(ConcatMatch _ (ConcatMatch (ExpMatch e m _) _))	-> (ExpMatch e m BracketBinding)) 
								(match (Concat startbracket (Concat (Exp ops) endbracket)) z))
								
		functionexps	=	(maybeMap (\l -> case l of
									(ConcatMatch func (ConcatMatch _ arg))		->	funcCombine func arg)) $
								(match (Concat (Exp ops) $ Concat WhiteSpace (Exp ops)) z)
								
		operatorexps	=	(maybeMap (\l -> case l of
									(ConcatMatch arg1 (ConcatMatch _ (ConcatMatch (WordExpMatch op) (ConcatMatch _ arg2))))
										|	isOperator op ops	->		case (operatorInfo op ops, arg1, arg2) of
												(Just opinfo, ExpMatch a1e a1m arg1b, ExpMatch a2e a2m arg2b)
													|	canOperatorBind arg1b opinfo arg2b	-> (
															case (funcCombine (case (funcCombine
															(ExpMatch (Variable 0) (Map.singleton op 0) BracketBinding)
															(ExpMatch a1e a1m BracketBinding)) of (Just l) -> l) 
															(ExpMatch a2e a2m BracketBinding)) of 
																Just (ExpMatch e m _) -> (
																	Just $ ExpMatch e m (OperatorBinding (fst opinfo) (snd opinfo))))
												_														->	Nothing
										|	otherwise			->		Nothing
								) (match (Concat (Exp ops) $ Concat WhiteSpace $ Concat (WordExp) $ Concat WhiteSpace (Exp ops)) z))
	
		res	=	Set.union wordexps $
					Set.union bracketexps $
					Set.union functionexps $
					Set.union operatorexps $
					Set.empty
						