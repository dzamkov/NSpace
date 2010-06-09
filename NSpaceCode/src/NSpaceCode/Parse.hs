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

import qualified Data.Set as Set
import qualified Data.Map as Map

-- A portion of an input text given a representation

data Token	=
	Character Char |
	Space |
	NewLine Int |		-- Indents included
	StringLiteral [Char] |
	Word [Char] deriving (Show)

-- Converts a string to a set of character tokens
	
tokenize		::	String -> [Token]
tokenize []			=	[]
tokenize (x:xs)	=	(Character x):(tokenize xs)

-- Parses a string of tokens using a stream function

streamParse			::	a -> (a -> Maybe Token -> (a, [Token])) -> [Token] -> [Token]
streamParse i f t	=	(snd nls) ++ (snd (f (fst nls) Nothing))
	where
		foldFunc	::	(a -> Maybe Token -> (a, [Token])) -> (a, [Token]) -> Token -> (a, [Token])
		foldFunc f (curState, curString) t	=	(fst nextState, curString  ++ snd nextState)
			where
				nextState = f (curState) (Just t)
		nls	=	(foldl (foldFunc f) (i, []) t)
	
-- (Current String, In Escape)
stringLiteralInitial		=	(Nothing, False)

-- Gives the final character for a char in the form "\x"
escapeChar	::	Char -> Char
escapeChar '\"'	=	'\"'
escapeChar '\\'	=	'\\'
escapeChar 'n'		=	'\n'
escapeChar 't'		=	'\t'

stringLiteralParse (Nothing, _) (Just (Character c))		=	if		c == '\"'
																				then	((Just "", False), [])
																				else	((Nothing, False), [Character c])
stringLiteralParse (Just s, False) (Just (Character c))	=	if		c == '\\'
																				then	((Just s, True), [])
																				else	if		c == '\"'
																						then	((Nothing, False), [StringLiteral s])
																						else	((Just (s ++ [c]), False), [])
stringLiteralParse (Just s, True) (Just (Character c))	=	((Just (s ++ [escapeChar c]), False), [])
stringLiteralParse (Just s, _) _									=	((Nothing, False), [StringLiteral s])
stringLiteralParse (Nothing, _) _								=	((Nothing, False), [])
	
-- (Current Indent Size, In WhiteSpace)
whiteSpaceInitial		=	(Nothing, False)

whiteSpaceParse (Nothing, _) (Just (Character ' '))		=	((Nothing, True), [])
whiteSpaceParse (Nothing, _) (Just (Character '\r'))		=	((Just 0, True), [])
whiteSpaceParse (Nothing, _) (Just (Character '\n'))		=	((Just 0, True), [])
whiteSpaceParse (Nothing, True) (Just x)						=	((Nothing, False), [Space, x])
whiteSpaceParse (Nothing, False) (Just x)						=	((Nothing, False), [x])
whiteSpaceParse (Just _, True) (Just (Character '\r'))	=	((Just 0, True), [])
whiteSpaceParse (Just _, True) (Just (Character '\n'))	=	((Just 0, True), [])
whiteSpaceParse (Just x, True) (Just (Character '\t'))	=	((Just (x + 1), True), [])
whiteSpaceParse (Just x, True) (Just y)						=	((Nothing, False), [NewLine x, y])
whiteSpaceParse (_, _) _											=	((Nothing, False), [])
			
-- (Current Word)
wordInitial		=	(Nothing) :: (Maybe String)

wordParse (Nothing) (Just (Character c))	=	((Just [c]), [])
wordParse (Nothing) (Just x)					=	((Nothing), [x])
wordParse (Nothing) (Nothing)					=	((Nothing), [])
wordParse (Just s) (Just (Character c))	=	((Just (s ++ [c])), [])
wordParse (Just s) (Just x)					=	((Nothing), [Word s, x])
wordParse (Just s) Nothing						=	((Nothing), [Word s])
			
-- Parses a text to the highest-level tokens possible.
				
parse	::	String -> [Token]
parse x	=	(streamParse wordInitial wordParse)
				$ (streamParse whiteSpaceInitial whiteSpaceParse) 
				$ (streamParse stringLiteralInitial stringLiteralParse) 
				$ tokenize x