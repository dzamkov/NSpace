-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Parse
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Parse (
	Accum(..),
	Parser(..),
	parse,
	sat,
	char,
	item,
	possible,
	string,
	most,
	least,
	multiple,
	newline,
	delimit,
	compose,
	anystring,
	linesplit,
	prefer,
	end,
	whiteSpace,
	Direction(..),
	OperatorLookup(..),
	ModifierLookup(..),
	listOperatorLookup,
	listModifierLookup,
	defaultOperators,
	defaultModifiers,
	expr
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified List as List
import NSpaceCode.Expression
import Control.Monad

-- Takes a string of the specified type and produces symbols (or tokens) after some amount
-- of characters are accepted. The remaining characters are also returned after a symbol is
-- produced.
newtype Accum a b	=	Accum ([a] -> [(b, [a])])

-- Parses a string and returns valid matches paired with a suffix string. The returned strings
-- are in order of size.

type Parser a		=	Accum Char a

parse (Accum a)	=	a

instance Functor (Accum a) where
	fmap f p	=	Accum (\cs -> map (\l -> (f (fst l), snd l)) (parse p cs))

instance Monad (Accum a) where
	return a	=	Accum (\cs -> [(a, cs)])
	p >>= f	=	Accum (\cs -> concat [parse (f a) cs' |
						(a, cs') <- parse p cs])
						
instance MonadPlus (Accum a) where
	mzero			=	Accum (\cs -> [])
	mplus x y	=	Accum (\cs -> (parse x cs) ++ (parse y cs))

-- Parses a character that satisfies the specified condition
sat		::	(a -> Bool) -> Accum a a
sat c		=	Accum (\cs ->	case cs of
					(char:tail)
						|	c char		->	[(char, tail)]
						|	otherwise	->	[]
					[]						->	[])
				
-- Parses a specific character				
char		::	(Eq a) => a -> Accum a a
char c	=	sat (\l -> l == c)	

-- Parses any character
item		:: Accum a a
item		=	sat (\l -> True)

-- Parses a specific string
string			::	(Eq a) => [a] -> Accum a [a]
string (c:s)	=	do
							char c
							string s
							return (c:s)
string []		=	return []
	
-- Parses any of the specified sequences.
union				::	[Accum a b]	->	Accum a b
union (p:ps)	=	mplus p (union ps)
union	[]			=	mzero
	
-- Parse something, or not
possible		::	Accum a b -> Accum a (Maybe b)
possible	p	=	mplus (return Nothing) (fmap (\l -> Just l) p)

-- Parses any string
anystring	::	Accum a [a]
anystring	=	multiple item

-- Parses as much as possible.
most		::	Accum a b -> Accum a b
most p	=	Accum (\cs -> [last (parse p cs)])

-- Parses the smallest string possible
least		::	Accum a b -> Accum a b
least p	=	Accum (\cs -> [head (parse p cs)])

-- Parses 0 or more of something
multiple		::	Accum a b -> Accum a [b]
multiple p	=	do
						r	<-	possible p
						case r of
							(Just x)	->	do
												rs	<-	multiple p
												return (x:rs)
							Nothing	->	return []
							
-- Composes two accumulators of different types.
compose		::	Accum a [b] -> Accum b c -> Accum a c
compose x y	=	Accum (\cs -> foldl (\ac it -> case it of
						(bstream, aremain)	->	foldl (\ac it -> case it of
								(cres, [])	->	((cres, aremain):ac)
								(_, _)		->	ac
							) ac (parse y bstream)
					) [] (parse x cs))
							
-- Parses a newline delimiter
newline	::	Parser ()
newline	=	fmap (\l -> ()) $ union [string "\r\n", string "\n"]
					
-- Parses a delimited string
delimit	:: Accum a b -> Accum a () -> Accum a [b]
delimit m d	=	do
						mr		<-	m
						dr		<-	possible d
						case dr of
							(Just _)	->	do
												rs	<-	delimit m d
												return (mr:rs)
							Nothing	->	return [mr]
							
-- Acts like the first specified parser if the second fails. Acts like the second
-- if it succeeds.
prefer		::	Accum a b -> Accum a b -> Accum a b
prefer x y	=	Accum (\cs	->	case (parse y cs) of
						[]		->	parse x cs
						y		->	y)
						
-- Forces the parsing of the end of a string
end	::	Accum a ()
end	=	Accum (\cs -> case cs of
				[]	->	[((), [])]
				_	->	[])
						
-- Parses a program into lines including indentation information and comment stripping.
linesplit	::	Parser [(Int, String)]
linesplit	=	do
						lines	<-	makelines
						return $ map (\l -> fst $ last $ parse parseline l) lines
	where
		makelines	=	delimit (multiple $ sat (\l -> case l of
								'\n'	->	False
								'\r'	->	False
								_		->	True)) newline
		parseline	=	do
								tabs	<-	most $ multiple $ char '\t'
								line	<-	prefer (multiple item) (do
												line <- multiple item
												string "--"
												multiple item
												return line)	
								return (length tabs, line)
								
-- Parses whitespace
whiteSpace	::	Parser ()
whiteSpace	=	fmap (\l -> ()) $ multiple $ sat (\l -> case l of
						' '	->	True
						'\n'	->	True
						'\r'	->	True
						'\t'	->	True
						_		->	False)
			
-- Parses a string that can act as a variable			
variable	::	Parser String
variable	=	multiple $ sat (\l -> Set.member l $ Set.fromList $
					['A'..'Z'] ++
					['a'..'z'] ++
					"+-=_*&^%$#@!|<>?:`/~")
								
-- Direction for associativy and modifiers
data	Direction	=	LeftDir
						|	RightDir deriving(Show, Eq)
						
-- Operator lookup information
data	OperatorLookup a	=	OperatorLookup	{
				getOperator	::	String -> Maybe a,
				precedence	::	a -> a -> Direction }
				
-- Modifier lookup information
data	ModifierLookup a	=	ModifierLookup	{
				getModifier		::	String -> Maybe a,
				modDirection	::	a -> Direction }
				
-- Expression obtained from parsing
type ParsedExpression	=	Expression (Either String Literal)
		
-- Intermediate expression parse information.
data ParseTree a b	=	Operator 		a (ParseTree a b) (ParseTree a b)
							|	Modifier 		b String (ParseTree a b)
							|	Application		(ParseTree a b) (ParseTree a b)
							|	RawExpression	ParsedExpression
							
-- Creates an operator lookup based on a list (ordered by bind strength) of operators.
listOperatorLookup		::	[(Direction, [String])]	->	OperatorLookup (Int, Direction)
listOperatorLookup ops	=	OperatorLookup go po
	where
		opmap	=	foldl (\ac it -> case it of
						(cur, (dir, strs))	->	foldl (\ac it -> Map.insert it (cur, dir) ac) ac strs 
					) Map.empty (zip [0..] ops)
		
		go x							=	Map.lookup x opmap
		po (l, ldir) (r, rdir)
			|	l < r								=	LeftDir
			|	r > l								=	RightDir
			|	l == r && ldir == LeftDir	=	LeftDir
			|	otherwise						=	RightDir
			
-- Creates a modifier lookup based on a list of modifiers.
listModifierLookup		::	[(Direction, String)]	->	ModifierLookup Direction
listModifierLookup mods	=	ModifierLookup gm id
	where
		modmap	=	foldl (\ac it -> case it of
							(dir, str)	->	Map.insert str dir ac
						) Map.empty mods
		gm x		=	Map.lookup x modmap
		
-- Gets the expression meaning of a string, if it has one.		
meaning	::	String -> Maybe (Expression Literal)
meaning "V"				=	Just $ Term UniversalL
meaning "universal"	=	Just $ Term UniversalL
meaning "="				=	Just $ Term EqualL
meaning "equal"		=	Just $ Term EqualL
meaning "+"				=	Just $ Term PlusL
meaning "add"			=	Just $ Term PlusL
meaning "-"				=	Just $ Term MinusL
meaning "subtract"	=	Just $ Term MinusL
meaning "*"				=	Just $ Term TimesL
meaning "multiply"	=	Just $ Term TimesL
meaning "/"				=	Just $ Term DivideL
meaning "divide"		=	Just $ Term DivideL
meaning "&"				=	Just $ Term AndL
meaning "and"			=	Just $ Term AndL
meaning "|"				=	Just $ Term OrL
meaning "or"			=	Just $ Term OrL
meaning "xor"			=	Just $ Term XorL
meaning "xand"			=	Just $ Term XandL
meaning "ite"			=	Just $ Term ITEL
meaning "ifthenelse"	=	Just $ Term ITEL
meaning "not"			=	Just $ Term NotL
meaning "solve"		=	Just $ Term SolveL
meaning "forall"		=	Just $ Term ForallL
meaning "exists"		=	Just $ Term ExistsL
meaning "lambda"		=	Just $ Lambda (Term Nothing)
meaning "identity"	=	Just $ Lambda (Term Nothing)
meaning x				=	Nothing

-- List of default operators
defaultOperators	=	[
	(LeftDir, ["*", "/"]),
	(LeftDir, ["+", "-"]),
	(LeftDir, ["&", "|"]),
	(LeftDir, ["="]),
	(LeftDir, ["or", "xor"]),
	(LeftDir, ["and", "xand"])]
	
-- List of default modifiers
defaultModifiers	=	[
	(RightDir, "forall"),
	(RightDir, "exists"),
	(RightDir, "lambda"),
	(RightDir, "solve")]

-- Parses an expression given an operator/modifier lookup and a leading term (expression applied at the
-- end of the string).
expr	::	OperatorLookup a -> ModifierLookup b -> Maybe ParsedExpression -> Parser ParsedExpression
expr ops mods lead	=	term
	where
		-- A symbol not based on other positionally-dependant symbols.
		atom	=	union	[
								(do
									v	<-	variable
									return $ Term $ Left v),
								(do
									char '('
									exp	<-	expr ops mods Nothing
									char ')'
									return exp),
								(do
									possible whiteSpace
									end
									case lead of
										(Just x)	->	return x
										Nothing	->	mzero)]
	
		-- Atoms stringed together as functions that are bound more tightly than operators.
		term	=	do
						atoms	<-	delimit atom whiteSpace
						case atoms of
							[]			->	mzero
							(x:xs)	->	return $ foldl (\ac it -> Function ac it) x xs