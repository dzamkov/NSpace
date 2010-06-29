-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Parse
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Parse (
	Parser(..),
	Operators(..),
	Associativity(..),
	defaultOperators,
	parse,
	item,
	sat,
	char,
	string,
	possible,
	multiple,
	whiteSpace
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression
import NSpaceCode.Value

-- Parses a string and returns valid matches paired with a suffix string

newtype Parser a	=	Parser (String -> [(a, String)])

parse (Parser a)	=	a

instance Monad Parser where
	return a	=	Parser (\cs -> [(a, cs)])
	p >>= f	=	Parser (\cs -> concat [parse (f a) cs' |
						(a, cs') <- parse p cs])
						
-- The result of an expression parse
						
data ParsedExp	=	ParsedExp (Expression SimpleCons) (Map.Map String Int) Binding
						
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


--- Operator binding 	-	binding created by an operator
--- Word binding			-	default binding to a single word
--- Bracket binding		-	binding caused by ( ), strongest
						
-- Never matches anything

none	::	Parser a
none	=	Parser (\cs -> [])
						
-- Parses a single character

item	::	Parser Char
item	=	Parser (\cs -> case cs of
				""			->	[]
				(c:cs)	->	[(c, cs)])
				
-- Parses all characters that satisfy the predicate

sat	::	(Char -> Bool) -> Parser Char
sat f	=	do
				c	<-	item
				if f c then return c else none 

-- Parses the specified character				
			
char		::	Char -> Parser Char
char c	=	sat (\l -> l == c)
				
--	Parses the specified string

string			::	String -> Parser String
string ""		=	return ""
string (c:cs)	=	do
							char c
							string cs
							return (c:cs)
							
-- Parses the specified pattern or nothing

possible		::	Parser a -> Parser (Maybe a)
possible x	=	Parser (\l -> (Nothing, l):(map (\m -> (Just $ fst m, snd m)) (parse x l))) 
							
-- Parses zero or more of the specified pattern

multiple		::	Parser a -> Parser [a]
multiple x	=	do
						r	<-	possible x
						case r of
							(Just l)	->	do
												rs	<-	multiple x
												return (l:rs)
							Nothing	->	return []
							
-- Parses some amount of whitespace

whiteSpace	::	Parser ()
whiteSpace	=	do
						r	<-	multiple (sat (\l ->	case l of
									' '	->	True
									'\t'	->	True
									'\n'	->	True
									_		->	False))
						if	length r > 0 then return () else none