-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Parse
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Parse (
	Parser(..),
	parse,
	item,
	sat,
	char,
	string
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