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
	Bracket Int Bool |
	Seperator |
	Forall (Set.Set [Char]) |
	Word [Char] |
	Block [[Token]] |
	Bracketed Int [Token] deriving (Show, Eq)
	
defaultOperators	=	[
	(True, Set.fromList (["*"])),
	(True, Set.fromList (["+", "-"])),
	(True, Set.fromList (["not"])),
	(True, Set.fromList (["="])),
	(True, Set.fromList (["or"])),
	(True, Set.fromList (["and"]))]

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
	
-- Parse function that simply replaces tokens.
	
replaceParse	::	(Token -> Token) -> () -> Maybe Token -> ((), [Token])
replaceParse f _ (Just x)	=	((), [f x])
replaceParse _ _ (Nothing)	=	((), [])
	
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

isWhiteSpace	::	Token -> Bool
isWhiteSpace (NewLine _)	=	True
isWhiteSpace (Space)			=	True
isWhiteSpace _					=	False

-- (Has Leading "-", In Comment)
commentInitial	=	(False, False)

commentParse (False, False) (Just (Character '-'))		=	((True, False), [])
commentParse (False, False) (Just x)						=	((False, False), [x])
commentParse (True, False) (Just (Character '-'))		=	((False, True), [])
commentParse (True, False) (Just x)							=	((False, False), [Character '-', x])
commentParse (False, True) (Just (NewLine x))			=	((False, False), [NewLine x])
commentParse (False, True)	(Just _)							=	((False, True), [])
commentParse (True, False) Nothing							=	((False, False), [Character '-'])
commentParse _ Nothing											=	((False, False), [])

--	0	=	()
-- 1	=	{}
--	2	=	[]

bracketInitial	=	() 

bracketParse	=	replaceParse (\x -> case x of
	Character '('		->		Bracket 0 False
	Character '{'		->		Bracket 1 False
	Character '['		->		Bracket 2 False
	Character ')'		->		Bracket 0 True
	Character '}'		->		Bracket 1 True
	Character ']'		->		Bracket 2 True
	y						->		y)

-- (Pre WhiteSpace, Seperator)

seperatorInitial	=	()

seperatorParse =	replaceParse (\x -> case x of
	Character ','		->		Seperator
	y						->		y)
			
-- (Current Word)
wordInitial		=	(Nothing) :: (Maybe String)

wordParse (Nothing) (Just (Character c))	=	((Just [c]), [])
wordParse (Nothing) (Just x)					=	((Nothing), [x])
wordParse (Nothing) (Nothing)					=	((Nothing), [])
wordParse (Just s) (Just (Character c))	=	((Just (s ++ [c])), [])
wordParse (Just s) (Just x)					=	((Nothing), [Word s, x])
wordParse (Just s) Nothing						=	((Nothing), [Word s])

-- (Current Word Set, Accepting new)
forallInitial	=	(Nothing, False)

forallParse (Nothing, False) (Just (Word "forall"))	=	((Just (Set.empty), True), [])
forallParse (Nothing, False) (Just x)						=	((Nothing, False), [x])
forallParse (Nothing, False) (Nothing)						=	((Nothing, False), [])
forallParse (Just y, True) (Just (Word x))				=	((Just (Set.insert x y), False), [])
forallParse (Just y, a) (Just (Space))						=	((Just y, a), [])
forallParse (Just y, False) (Just (Seperator))			=	((Just y, True), [])
forallParse (Just y, False) (Just x)						=	((Nothing, False), [Forall y, x])
forallParse (Just y, _) (Nothing)							=	((Nothing, False), [])

-- Bracket parsing is complicated
bracketedInitial	:: [(Int, [Token])]
bracketedInitial	=	[]

bracketedParse [] (Just (Bracket x False))					=	([(x, [])], [])
bracketedParse [] (Just x)											=	([], [x])
bracketedParse [] Nothing											=	([], [])
bracketedParse ((y, toks):[]) (Just (Bracket _ True))		=	([], [Bracketed y toks]) 
bracketedParse (x:y) z												=	case (bracketedParse y z) of
																					(ninfo, toks)	->	case x of
																						(i, l)	->	((i, l ++ toks):ninfo, [])

-- Parses a text to the highest-level tokens possible.
				
parse	::	String -> [Token]
parse x	=	(streamParse bracketedInitial bracketedParse)
				$ (streamParse forallInitial forallParse)
				$ (streamParse wordInitial wordParse)
				$ (streamParse seperatorInitial seperatorParse)
				$ (streamParse bracketInitial bracketParse)
				$ (streamParse commentInitial commentParse)
				$ (streamParse whiteSpaceInitial whiteSpaceParse)
				$ (streamParse stringLiteralInitial stringLiteralParse) 
				$ tokenize x