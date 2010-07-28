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
	sat,
	char,
	item,
	possible
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified List as List
import NSpaceCode.Expression
import Control.Monad

-- Parses a string and returns valid matches paired with a suffix string. The returned strings
-- are in order of size.

newtype Parser a	=	Parser (String -> [(a, String)])

parse (Parser a)	=	a

instance Functor Parser where
	fmap f p	=	Parser (\cs -> map (\l -> (f (fst l), snd l)) (parse p cs))

instance Monad Parser where
	return a	=	Parser (\cs -> [(a, cs)])
	p >>= f	=	Parser (\cs -> concat [parse (f a) cs' |
						(a, cs') <- parse p cs])
						
instance MonadPlus Parser where
	mzero			=	Parser (\cs -> [])	
	mplus x y	=	Parser (\cs -> (parse x cs) ++ (parse y cs))

-- Parses a character that satisfies the specified condition
sat		::	(Char -> Bool) -> Parser Char
sat c		=	Parser (\cs ->	case cs of
					(char:tail)
						|	c char		->	[(char, tail)]
						|	otherwise	->	[]
					[]						->	[])
				
-- Parses a specific character				
char		::	Char -> Parser Char
char c	=	sat (\l -> l == c)	

-- Parses any character
item		::	Parser Char
item		=	sat (\l -> True)
	
-- Parse something, or not
possible		::	Parser a -> Parser (Maybe a)
possible	p	=	mplus (fmap (\l -> Just l) p) (return Nothing)