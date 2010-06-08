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
		pws (Nothing, ws, t) (Character c)	=	if		c == ' '
															then	(Nothing, True, t)
															else	if		c == '\n' || c == '\r'
																	then	if		ws
																			then	(Just 0, True, t ++ [Space])
																			else	(Just 0, True, t)
																	else	if		ws
																			then	(Nothing, False, t ++ [Space, Character c])
																			else	(Nothing, False, t ++ [Character c])
		pws (Just x, True, t) (Character c)	=	if		c == '\t'
															then	(Just (x + 1), True, t)
															else	(Nothing, False, t ++ [NewLine x])
		pws (Nothing, ws, t) x					=	if		ws
															then	(Nothing, False, t ++ [Space, x])
															else	(Nothing, False, t ++ [x])
		pws (Just x, True, t) y					=	(Nothing, False, t ++ [NewLine x, y])
		
		res	=	case foldl (\x y -> pws x y) (Nothing, False, []) input of
			(_, _, x)	->	x

				
-- Parses a text to the highest-level tokens possible.
				
parse	::	String -> [Token]
parse x	=	parseWhiteSpace $ parseStringLiterals $ tokenize x