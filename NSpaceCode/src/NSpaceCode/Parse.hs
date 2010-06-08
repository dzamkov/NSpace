-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Parse
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Parse (
	Token(..),
	tokenize,
	parse
) where 

-- A portion of an input text given a representation

data Token	=
	Character Char |
	Space |
	NewLine Int |		-- Indents included
	StringLiteral [Char] deriving (Show)

-- Converts a string to a set of character tokens
	
tokenize		::	String -> [Token]
tokenize []			=	[]
tokenize (x:xs)	=	(Character x):(tokenize xs)
	
-- Finds the string literals in a given input.
	
parseStringLiterals			::	[Token] -> [Token]
parseStringLiterals input	=	res
	where 
	
		-- Gives the final character for a char in the form "\x"
		escapeChar	::	Char -> Char
		escapeChar '\"'	=	'\"'
		escapeChar '\\'	=	'\\'
		escapeChar 'n'		=	'\n'
		escapeChar 't'		=	'\t'
	
		-- (Current String Literal, In Escape, Current Token Set)
		-- Chars given in order as they appear in the text
		psl	::	(Maybe String, Bool, [Token]) -> Char -> (Maybe String, Bool, [Token])
		psl (Nothing, _, x) y		=	if		y == '\"'
												then	(Just "", False, x)
												else	(Nothing, False, x ++ [Character y])
		psl (Just x, False, t) y	=	if		y == '\\'
												then	(Just x, True, t)
												else	if 	y == '\"'
														then	(Nothing, False, t ++ [StringLiteral x])
														else	(Just (x ++ [y]), False, t)
		psl (Just x, True, t) y		=	(Just (x ++ [escapeChar y]), False, t)
		
		res	=	case foldl (\x y -> case y of
			Character c		->	psl x c
			_					->	undefined) (Nothing, False, []) input of
				(_, _, x)		->		x


-- Parses white space in a string of tokens as either spaces or newlines.
parseWhiteSpace			::	[Token] -> [Token]
parseWhiteSpace input	=	res
	where
		-- (Current Newline Ident Size, In WhiteSpace, Current Token Set)
		pws	::	(Maybe Int, Bool, [Token]) -> Token -> (Maybe Int, Bool, [Token])
		pws (Nothing, _, t) (Character ' ')		=	(Nothing, True, t)
		pws (Nothing, _, t) (Character '\r')	=	(Just 0, True, t)
		pws (Nothing, _, t) (Character '\n')	=	(Just 0, True, t)
		pws (Nothing, True, t) x					=	(Nothing, False, t ++ [Space, x])
		pws (Nothing, False, t) x					=	(Nothing, False, t ++ [x])
		pws (Just _, True, t) (Character '\n')	=	(Just 0, True, t)
		pws (Just x, True, t) (Character '\t')	=	(Just (x + 1), True, t)
		pws (Just x, True, t) y						=	(Nothing, False, t ++ [NewLine x, y])
		
		res	=	case foldl (\x y -> pws x y) (Nothing, False, []) input of
			(_, _, x)	->	x

				
-- Parses a text to the highest-level tokens possible.
				
parse	::	String -> [Token]
parse x	=	parseWhiteSpace $ parseStringLiterals $ tokenize x