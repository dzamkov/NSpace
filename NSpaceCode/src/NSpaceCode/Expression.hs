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
	replace,
	match,
	patternize,
	compute,
	condense,
	isComplete,
	instintate,
	produceIntermediate
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
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

--	A type of modifier, an expression which describes a relation on a variable.
	
data ModifierType	=
	Forall	|
	Exists	|
	Lambda	|
	Solve		deriving(Show, Ord, Eq)
	
-- A relation between variables, and functions
-- that produces a definite value.

data Expression a	=	
	Variable a											|
	Constant Literal									|
	Function (Expression a) (Expression a)		|
	Modifier	ModifierType (Expression a) a		deriving (Show, Eq, Ord)
	
-- Estimates the complexity of an expression.
complexity	::	Expression a -> Integer
complexity (Variable _)				=	1
complexity (Constant _)				=	0
complexity (Function l m)			=	(complexity l) + (complexity m) + 1
complexity (Modifier Lambda l _)	=	(complexity l)
complexity (Modifier _ l _)		=	(complexity l) * 2

-- Creates an expression of ints that is functionally equivalent to the
-- specified expression in such a way that all expressions that are equivalent
-- with different variable mappings will have an equivalent (by ==) condensed form.
condense	::	(Ord a) => Expression a -> (Expression Int, Map.Map a Int)
condense	e	=	res
	where
		scondense nf mp (Variable x)		=	case Map.lookup x mp of
															(Just l)	-> (Variable 0, mp, nf)
															Nothing	->	(Variable nf, 
																Map.insert x nf mp, nf + 1)
		scondense nf mp (Constant c)		=	(Constant c, mp, nf)
		scondense nf mp (Function l m)	=	res
			where
				lcon	=	scondense nf mp l
				mcon	=	case lcon of (_, y, z)	->	scondense z y m
				res	=	case (lcon, mcon) of
								((le, _, _), (me, nmp, nnf))	->	(Function le me, nmp, nnf)
		scondense nf mp (Modifier m e v)	=	case scondense nf mp e of
															(ne, nmp, nnf)	->	case Map.lookup v nmp of
																(Just x) -> (Modifier m ne x, Map.delete v nmp, nnf)
		res 	=	case scondense 0 Map.empty e of (x, y, _) -> (x, y)
	
-- Replaces some variable with another in an expression.
replace	::	(Eq a) => a -> a -> Expression a -> Expression a
replace t r (Variable x)
	|	x == t						=	Variable r
	|	otherwise					=	Variable x
replace _ _ (Constant c)		=	Constant c
replace t r (Function l m)		=	Function (replace t r l) (replace t r m)
replace t r (Modifier m e v)
	|	t == v						=	Modifier m e v
	|	otherwise					=	Modifier m (replace t r e) v
	
-- Replaces all variables in an expression. 1 to 1 only please.
replaceAll	::	(a -> b) -> Expression a -> Expression b
replaceAll f (Variable x)		=	Variable (f x)
replaceAll f (Constant c)		=	Constant c
replaceAll f (Function l m)	=	Function (replaceAll f l) (replaceAll f m)
replaceAll f (Modifier m e v)	=	Modifier m (replaceAll f e) (f v)
	
-- The type of a variable in a pattern
type PatternVar a	=	Either Int a
	
-- A pattern is an expression with some terms missing, being instead replaced
-- by a free variable.
type Pattern a		=	Expression (PatternVar a)

-- Converts an expression into a pattern based on a list of free variables in
-- the pattern.
patternize	::	(Ord a) => Expression a -> [a] -> Pattern a
patternize e l		=	res
	where
		varmap	=	snd $ foldl (\ac it -> (fst ac + 1, Map.insert it (fst ac) (snd ac))) (0, Map.empty) l
	
		subpat vmap (Variable var)		=	case Map.lookup var vmap of
														(Just x)		->		Variable $ Left x
														Nothing		->		Variable $ Right var
		subpat vmap (Constant c)		=	Constant c
		subpat vmap (Function q r)		=	Function (subpat vmap q) (subpat vmap r)	
		subpat vmap (Modifier m e v)	=	Modifier m (subpat (Map.delete v vmap) e) (Right v)

		res	=	subpat varmap e
		
-- Patternizes a pattern, allowing it to be matched against other patterns.
		
weakPatternize	:: Pattern a -> Pattern (PatternVar a)
weakPatternize	(Variable (Right x))	=	(Variable (Right (Right x)))
weakPatternize (Variable (Left x))	=	(Variable (Left x))
weakPatternize (Constant x)			=	(Constant x)
weakPatternize (Function x y)			=	(Function (weakPatternize x) (weakPatternize y))
weakPatternize (Modifier m e p)		=	(Modifier m (weakPatternize e) (Right p))

-- Information about what "fits" into the free variables of a pattern. A pattern
-- combined with an instance can be used to generate an expression.

type Instance a	=	Map.Map Int (Expression a)

instintate	::	Pattern a -> Instance a -> Expression a
instintate (Variable (Left x)) m			=	case Map.lookup x m of (Just l) -> l
instintate (Variable (Right x)) _		=	Variable x
instintate (Constant c) _					=	Constant c
instintate (Function x y) m				=	Function (instintate x m) (instintate y m)
instintate (Modifier t e (Right v)) m	=	Modifier t (instintate e m) v
		
-- Tries really hard to convert an expression to the form of a pattern, creating an instance
-- that when applied to the pattern, gets the original expression.
		
match		::	(Eq a) => Pattern a -> Expression a -> Maybe (Instance a)
match (Variable (Left x)) y									=	Just (Map.singleton x y)
match (Variable (Right x)) (Variable y)
	|	x == y														=	Just (Map.empty)
	|	otherwise													=	Nothing
match (Variable (Right _)) _									=	Nothing
match (Constant x) (Constant y)							
	|	x == y														=	Just (Map.empty)
	|	otherwise													=	Nothing
match (Function aa ba) (Function ab bb)					=	res
	where
		res	=	case (match aa ab, match ba bb) of
						(Just l, Just m)	->	if		Map.fold (\it ac -> it && ac) True (
														Map.intersectionWith (\l m -> l == m) l m)
													then	Just (Map.union l m)
													else	Nothing
						_						->	Nothing
match (Function _ _) _											=	Nothing
match (Modifier m sp (Right p)) (Modifier bm bsp bp)
	|	m /= bm														=	Nothing
	|	p == bp														=	match sp bsp
	|	otherwise													=	match sp (replace bp p bsp)


-- A rule describes a possible equivalence between values. It has an option condition (set to true
--	if rule is unconditional) and two corresponding patterns known to be equivalent if the condition
-- is met. It also contains a mapping of variables in the three patterns.
		
data Rule a		=	Rule 
							(Pattern a) 
							(Pattern a) 
							(Pattern a) deriving(Show, Eq, Ord)
						
--	Represents a reduction query, which describes the take of converting an expression in one
-- form to another specified by a pattern using a set of rules.

data Query a	=	Query (Pattern a) (Expression a) deriving(Show, Eq, Ord)

-- Condesnses a query, for more efficent caching

condenseQuery	::	(Ord a) => Query a -> (Query Int, Map.Map a Int)
condenseQuery (Query p e)	=	res
	where
		econ	=	condense e
		res	=	case econ of
						(ne, emap)	->	(Query (replaceAll (\l -> case l of
							(Left x) 	-> Left x
							(Right x)	->	Right (case Map.lookup x emap of (Just y) -> y)) p)
							ne, emap)

-- Part of a query obtained from applying a single rule to a subquery.

data IntermediateQuery a	=	IntermediateQuery
											(Query a)
											(Instance (PatternVar a))	deriving(Show)

-- Smaller query derived from a larger one, with no knowledge of rules. This can
-- be done by simple pattern matching.					
	
data SubQuery a		=
	FunctionalSubQuery (Query a) (Query a)		|
	ModifierSubQuery ModifierType (Query a) a	deriving(Show, Eq, Ord)
											
-- Produces the intermediate queries that, when unioned, get the result of
-- the specified query.
											
produceIntermediate	::	(Eq a) => [Rule a] -> Query a -> [IntermediateQuery a]
produceIntermediate ((Rule cond rp lp):rm) (Query pat tar)	=	res
	where
		mkone rp lp	=	case match (weakPatternize pat) lp of
								(Just ins)	->	Just $ IntermediateQuery (Query rp tar) ins
								Nothing		->	Nothing
		res	=	case (mkone rp lp, mkone lp rp) of 
						(Just x, Just y)	->	x:y:(produceIntermediate rm (Query pat tar))
						(Just x, _)			->	x:(produceIntermediate rm (Query pat tar))
						(_, Just y)			->	y:(produceIntermediate rm (Query pat tar))
						(_, _)				->	(produceIntermediate rm (Query pat tar))
produceIntermediate [] _												=	[]

-- Produces a subquery for a query, if any ezists
produceSubQuery	::	(Eq a) => Query a -> Maybe (SubQuery a)
produceSubQuery (Query (Function l m) (Function x y))	=	Just $ FunctionalSubQuery (Query l x) (Query m y)
produceSubQuery (Query (Modifier pm pe pv) (Modifier tm te tv))	
	|	pm == tm														=	Just $ ModifierSubQuery pm (Query (replace pv (Right tv) pe) te) tv
produceSubQuery _													=	Nothing
	
-- Creates an unfilled query result for a query
produceQueryResult		::	(Eq a) => [Rule a] -> Query a -> QueryResult a
produceQueryResult r q	=	QueryResult (Set.empty) (produceIntermediate r q) (produceSubQuery q)
	
-- An intermediate result of a query

data QueryResult a			=	QueryResult 
											(Set.Set (Instance a)) 
											[IntermediateQuery a]
											(Maybe (SubQuery a))
											deriving(Show)				

-- Gets if a query result is complete and has no intermediate state.
isComplete	::	QueryResult a -> Bool
isComplete (QueryResult _ [] (Nothing))	=	True
isComplete (QueryResult _ _ _)				=	False

-- Cache of queries with corresponding results.

type QueryCache a				=	Map.Map (Query a) (QueryResult a)

-- Computes the result of a query using a cache and a set of rules. The integer argument specifies
-- the maximum complexity the computation should process before returning. The unused complexity
-- is returned.
compute	::	(Ord a) =>	[Rule Int] -> 
								QueryCache Int -> 
								Query a -> 
								Integer ->
								(Set.Set (Instance a), QueryCache Int, Integer)
compute r c q o	=	res
	where
		rules				=	r
		
		iqQueryPart		::	IntermediateQuery a -> Query a
		iqQueryPart (IntermediateQuery x _)	=	x
	
		scompute c q o e	= res
			where
				excluded			=	e
				cache				=	c
				query				=	q
				condensedinfo	=	condenseQuery query
				condensedquery	=	fst condensedinfo
				condensedmap	=	snd condensedinfo
				icondensedmap	=	Map.fromList $ map (uncurry (\l m -> (m, l))) $ Map.toList condensedmap
				currentqr		=	case Map.lookup condensedquery cache of
											(Just x)	->	x
											Nothing	->	produceQueryResult rules condensedquery
				needqueries		=	case currentqr of
											(QueryResult _ iqs (Just (FunctionalSubQuery l m)))	->	l:m:(map iqQueryPart iqs)
											(QueryResult _ iqs (Just (ModifierSubQuery _ l _)))	->	l:(map iqQueryPart iqs)
											(QueryResult _ iqs Nothing)									->	(map iqQueryPart iqs)
				scoredqueries	=	map (\l -> (l, case l of (Query pat exp) -> complexity exp `div` complexity pat)) needqueries
				sortedqueries	=	sortBy (\l m -> case (l, m) of ((_, ls), (_, ms)) -> compare ls ms) scoredqueries
				computedinfo	=	foldl (\ac it ->	(
											let	curo			=	(case ac of (x, _, _) -> x) - 1	in
											let	curcache		=	case ac of (_, x, _) -> x	in
											let	curresmap	=	case ac of (_, _, x) -> x	in
											let	curquery		=	fst it	in
											let	res			=	scompute curcache curquery curo (
																			Set.insert condensedquery e)	in
											let	nresmap		=	Map.insert curquery (case res of (x, _, _) -> x) curresmap	in
											let	ncache		=	case res of (_, x, _) -> x	in
											let	no				=	case res of (_, _, x) -> x
													in	(no, ncache, nresmap)
										)) (o, cache, Map.empty) sortedqueries
				needqueriesres	=	(case computedinfo of (_, _, x) -> x)	::	Map.Map (Query Int) (Set.Set (Instance Int))
				curreses			=	case currentqr of (QueryResult x _ _) -> x
				nreses			=	Set.union curreses $ foldl (\ac it -> case it of
											(IntermediateQuery nq rp)	->	Set.union ac $
																						let	insts	=	case Map.lookup nq needqueriesres of (Just x) -> x
																						in		Set.map (\l -> Map.map (\m -> instintate m l) rp) insts
										) Set.empty (case currentqr of (QueryResult _ x _) -> x)
				nnreses			=	Set.union nreses $ case currentqr of
											(QueryResult _ _ (Just (FunctionalSubQuery l m)))	->	case (Map.lookup l needqueriesres, Map.lookup m needqueriesres) of
												(Just ls, Just ms)	->	Set.fold (\curl ac ->
														Set.fold (\curm ac -> case condensedquery of
															(Query pat exp)	->	case match pat (Function curl curm
																							) of
																(Just x)	->	Set.insert x ac
																Nothing	->	ac
														) ac ms
													) Set.empty ls
											_																	->	Set.empty
		res	=	scompute c q o (Set.empty)