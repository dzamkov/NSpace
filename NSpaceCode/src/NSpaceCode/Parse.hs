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
	Direction(..),
	OperatorLookup(..),
	ModifierLookup(..)
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
								
-- Direction for associativy and modifiers
data	Direction	=	LeftDir
						|	RightDir
						
-- Operator lookup information
data	OperatorLookup	=	forall a.
			OperatorLookup	{
				getOperator	::	String -> Maybe a,
				precedence	::	a -> a -> Direction }
				
-- Modifier lookup information
data	ModifierLookup	=	forall a.
			ModifierLookup	{
				getModifier		::	String -> Maybe a,
				modDirection	::	a -> Direction }
		
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