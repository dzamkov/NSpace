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
	ParsedExpr(..),
	Associativity(..),
	defaultOperators,
	operatorInfo,
	isOperator,
	parse,
	item,
	sat,
	char,
	string,
	possible,
	union,
	multiple,
	whiteSpace,
	word,
	end,
	expr,
	amount
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
						
data ParsedExpr	=	ParsedExpr (Expression SimpleCons) (Map.Map String Int) deriving (Show, Eq)

functionCombine	::	ParsedExpr -> ParsedExpr -> ParsedExpr
functionCombine (ParsedExpr fe fm) (ParsedExpr ae am)	= res
	where
		intsect			=	Map.fromList $ Map.elems $ Map.intersectionWith (\l m -> (m, l)) fm am
		startunbound	=	(Set.findMax (getBound fe)) + 1
		newexp			=	Function fe $ rebind ae (\l ->	case Map.lookup l intsect of
									(Just m) -> m
									Nothing	-> l + startunbound)
		newmap			=	Map.unionWith (\l m -> l) fm $ (Map.map (\l -> l + startunbound) am)
		res				=	ParsedExpr newexp newmap
						
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

-- Parses any of the specified patterns

union				::	[Parser a] ->  Parser a
union []			=	none
union (p:ps)	=	Parser (\l -> (parse p l) ++ (parse (union ps) l))
							
-- Parses one or more of the specified pattern

multiple		::	Parser a -> Parser [a]
multiple x	=	do
						r	<- x
						rs	<-	possible (multiple x)
						case rs of
							(Just l)	->	return (r:l)
							Nothing	->	return [r]

-- Parses zero or more of the specified pattern							
		
amount	::	Parser a -> Parser [a]
amount x	=	do
					r	<-	possible x
					case r of
						(Just l)	->	do
											rs	<-	amount x
											return (l:rs)
						Nothing	->	return []
					

-- Parses some amount of whitespace

whiteSpace	::	Parser ()
whiteSpace	=	do
						multiple (sat (\l ->	case l of
									' '	->	True
									'\t'	->	True
									'\n'	->	True
									'\r'	->	True
									_		->	False))
						return ()
						
-- Only matches the end of a string

end	::	Parser ()
end	=	Parser (\l ->	if		l == ""
								then	[((), "")]
								else	[])
						
-- Parses a string made up of word characters

word	::	Parser String
word	=	do
				multiple $ sat (\l -> Set.member l wordChars)
		where
			wordChars	=	Set.fromList (['a'..'z'] ++ ['A'..'Z'] ++ "+_-=*&^%$@!~':|<>?")

-- Parses an expression	

type Term	=	Either ParsedExpr String
	
expr		::	Operators -> Parser ParsedExpr
expr ops	=	do
					terms	<-	termParse
					case (termReduce terms) of
						(Just x)	->	return x
						Nothing	->	none
			where
				termParse	::	Parser [Term]
				termParse	=	do
										union [
											(do
												r	<-	word
												rs	<-	conParse
												case operatorInfo r ops of
													Just (str, asc)	->	return ((Right r):rs)
													Nothing				->	return ((Left $ ParsedExpr (Variable 0) (Map.singleton r 0)):rs)),
											(do
												char '('
												possible whiteSpace
												e	<-	expr ops
												possible whiteSpace
												char ')'
												rs	<-	conParse
												case e of
													(ParsedExpr exp map)	->	return ((Left $ ParsedExpr exp map):rs))]
				conParse		::	Parser [Term]
				conParse		=	do
										w	<-	possible whiteSpace
										case w of
											(Just _)	->	termParse
											Nothing	->	return []
				termReduce		::	[Term] -> Maybe ParsedExpr
				termReduce x	=	case operatorReduce $ functionReduce x of
											[Left l]				->	Just l
											_						->	Nothing
					where
						functionReduce	((Left x):(Left y):cs)	=	functionReduce $ (Left $ functionCombine x y):cs	
						functionReduce (x:rs)						=	x:(functionReduce rs)
						functionReduce []								=	[]
						
						operatorReduce		::	[Term] -> [Term]
						operatorReduce x	=	foldl (\cur op -> case op of
														(LeftAssociative, opset)	->	laOpReduce cur opset) x ops
							where
								laOpReduce	::	[Term] -> Set.Set String -> [Term]
								laOpReduce ((Left f):(Right op):(Left a):rs) opset
									|	Set.member op opset	=	laOpReduce ((Left 
																			(functionCombine 
																				(functionCombine
																					(ParsedExpr (Variable 0) (Map.singleton op 0))
																					f)
																				a)):rs) opset
								laOpReduce (x:rs) opset	=	x:(laOpReduce rs opset)
								laOpReduce [] _			=	[]