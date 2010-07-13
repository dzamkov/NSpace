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
	modifier,
	stringLiteral,
	intLiteral,
	listLiteral,
	replaceVarC,
	amount,
	ignoreSpace,
	fileParse,
	quickParse,
	interpret
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression

-- Parses a string and returns valid matches paired with a suffix string

newtype Parser a	=	Parser (String -> [(a, String)])

parse (Parser a)	=	a

instance Monad Parser where
	return a	=	Parser (\cs -> [(a, cs)])
	p >>= f	=	Parser (\cs -> concat [parse (f a) cs' |
						(a, cs') <- parse p cs])
						
-- The result of an expression parse
						
data ParsedExpr	=	ParsedExpr Expression (Map.Map String Int) deriving (Show, Eq)

functionCombine	::	ParsedExpr -> ParsedExpr -> ParsedExpr
functionCombine (ParsedExpr fe fm) (ParsedExpr ae am)	= res
	where
		fsize			=	boundVars fe
		iam			=	Map.fromList $ map (\l -> case l of (a, b) -> (b, a)) $ Map.toList am
		
		fres			=	Map.foldWithKey (\k v a -> case a of
								(p, m, s)	->	case Map.lookup v fm of
									(Just l)		->	(p, m, Set.insert (l, k) s)
									Nothing		->	(p + 1, Map.insert v p m, s)
							) (0, Map.empty, Set.empty) iam
		
		nam			=	case fres of (_, l, _) -> l
		nm				=	case fres of (_, _, l) -> l
		res			=	ParsedExpr (Function fe ae nm) (Map.union fm $ Map.map (\l -> l + fsize) nam)
		
						
-- Operator information
	
data Associativity	=
	LeftAssociative |
	RightAssociative deriving (Show, Eq, Ord)
	
type Operators			=	[(Associativity, Set.Set String)]


defaultOperators		=	[
	(LeftAssociative, 	Set.fromList ["*"]),
	(LeftAssociative, 	Set.fromList ["+", "-"]),
	(LeftAssociative,		Set.fromList ["where"]),
	(LeftAssociative,		Set.fromList ["in"]),
	(LeftAssociative, 	Set.fromList [".."]),
	(LeftAssociative,		Set.fromList ["="]),
	(LeftAssociative,		Set.fromList ["|"]),
	(LeftAssociative,		Set.fromList ["or", "xor"]),
	(LeftAssociative,		Set.fromList ["&"]),
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
				
-- Ignored whitespace, including comments.				
ignoreSpace	::	Parser ()
ignoreSpace	=	union [whiteSpace,
						(do
							possible whiteSpace
							string "/*"
							amount item
							string "*/"
							possible ignoreSpace
							return ())]
						
						
-- Only matches the end of a string

end	::	Parser ()
end	=	Parser (\l ->	if		l == ""
								then	[((), "")]
								else	[])
						
-- Parses a string made up of word characters that can be used as a variable.
-- Modifiers are excluded.

word	::	Parser String
word	=	do
				w	<-	multiple $ sat (\l -> Set.member l wordChars)
				case w of
					"forall"	->	none
					"exists"	->	none
					"lambda"	->	none
					"solve"	->	none
					"if"		->	none
					"then"	->	none
					"else"	->	none
					x			->	return x
		where
			wordChars	=	Set.fromList (['a'..'z'] ++ ['A'..'Z'] ++ "+_-=*&^%$@!~':|<>?")
			
--	Parses a string literal

stringLiteral	::	Parser String
stringLiteral	=	do
							sqoute	<-	quotechar
							res		<-	instring sqoute
							eqoute	<-	quotechar
							if		sqoute == eqoute
								then	return res
								else	none
					where
						unescape '"'	=	'"'
						unescape '\''	=	'\''
						unescape 'n'	=	'\n'
						unescape 't'	=	'\t'
						unescape 'r'	=	'\r'
						quotechar	=	sat (\l -> case l of
							'"'	->	True
							'\''	->	True
							_		->	False)
						instring x	=	amount $ union [sat (\l -> l /= x),
							(do
								char '\\'
								ec	<-	item
								return $ unescape ec)]
								
--	Parses an integer literal

intLiteral	::	Parser Integer
intLiteral	=	do
						numchars	<-	multiple $ sat (\l -> Set.member l $ Set.fromList ['0'..'9'])
						return (read numchars)

-- Parses a list literal defined by the start and end chacters ('[' and ']')
						
listLiteral				::	Char -> Char -> Operators -> Parser [ParsedExpr]
listLiteral	s e ops	=	do
									char s
									possible whiteSpace
									listItems
							where
								listItems	=	do
														e	<-	expr ops
														rs	<-	conItems
														return (e:rs)
								conItems		=	union [
									(do
										possible whiteSpace
										char ','
										possible	whiteSpace
										listItems),
									(do
										possible whiteSpace
										char e
										return [])]
										
-- Parses a modifier (forall, lambda, exists, possible)
modifier		::	String -> Parser [String]
modifier x	=	do
						string x
						whiteSpace
						r	<-	word
						rs	<-	possible conModifier
						case rs of
							(Just l)	->	return (r:l)
							Nothing	->	return [r]
				where
					conModifier	=	do
											possible whiteSpace
											char ','
											possible whiteSpace
											r	<-	word
											rs	<-	possible conModifier
											case rs of
												(Just l)	->	return (r:l)
												Nothing	->	return [r]
						
						
-- Parses an expression	

type Term	=	Either ParsedExpr String
	
expr	::	Operators  -> Parser ParsedExpr
expr ops  =	union [
					(do
						terms	<-	termParse
						case (termReduce terms) of
							(Just x)	->	return x
							Nothing	->	none),
					(do
						string "if"
						possible ignoreSpace
						cond	<-	expr ops
						possible ignoreSpace
						string "then"
						possible ignoreSpace
						act	<-	expr ops
						nact	<-	possible $ (do
										possible ignoreSpace
										string "else"
										possible ignoreSpace
										expr ops)
						case nact of
							(Just l)	->	return $ functionCombine (functionCombine
								(functionCombine (ParsedExpr (Constant $ ITEL) Map.empty) cond) act) l 
							Nothing	->	return $ functionCombine (functionCombine
								(functionCombine (ParsedExpr (Constant $ ITEL) Map.empty) cond) act) 
								(ParsedExpr (Constant $ LogicL $ True) Map.empty)),
					modifierTest "forall" Forall,
					modifierTest "exists" Exists,
					modifierTest "lambda" Lambda,
					modifierTest "solve" Solve]
			where
				modifierTest			::	String -> ModifierType -> Parser ParsedExpr
				modifierTest s mod	=	do
												vars	<-	modifier s
												possible ignoreSpace
												e		<-	expr ops
												return $ foldl (\ac var -> case ac of
														(ParsedExpr pe pm)	->	case Map.lookup var pm of
															(Just l)			->	(ParsedExpr (Modifier mod pe l)
																(Map.map (\m ->	if		m > l
																						then	m - 1
																						else	m) $
																Map.delete var pm))
													) e vars
												
			
				termParse	::	Parser [Term]
				termParse	=	do
										term	<-	union [
											(do
												r	<-	word
												case operatorInfo r ops of
													Just (str, asc)	->	return (Right r)
													Nothing				->	return (Left $ ParsedExpr (Variable) (Map.singleton r 0))),
											(do
												char '('
												possible ignoreSpace
												e	<-	expr ops
												possible ignoreSpace
												char ')'
												case e of
													(ParsedExpr exp map)	->	return (Left $ ParsedExpr exp map)),
											(do
												str	<-	stringLiteral
												return (Left $ ParsedExpr (
													foldl (\c l -> 
														Function c (Constant $ CharL l) Set.empty) 
															(Constant $ ListL) str) Map.empty)),
											(do
												int	<-	intLiteral
												return (Left $ ParsedExpr (Constant $ IntegerL int) Map.empty)),
											(do
												li		<-	listLiteral '[' ']' ops
												return (Left $ foldl (\c l -> functionCombine c l) 
													(ParsedExpr (Constant ListL) Map.empty) li)),
											(do
												li		<-	listLiteral '{' '}' ops
												return (Left $ foldl (\c l -> functionCombine c l) 
													(ParsedExpr (Constant SetL) Map.empty) li))]
										rs	<-	conParse
										return (term:rs)
				conParse		::	Parser [Term]
				conParse		=	do
										w	<-	possible ignoreSpace
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
																					(ParsedExpr (Variable) (Map.singleton op 0))
																					f)
																				a)):rs) opset
								laOpReduce (x:rs) opset	=	x:(laOpReduce rs opset)
								laOpReduce [] _			=	[]
								
-- Replaces a variable in an expression with a constant
replaceVarC	::	String -> Literal -> ParsedExpr -> ParsedExpr
replaceVarC v t (ParsedExpr e m)	=	case Map.lookup v m of
	(Just l)		->	ParsedExpr (replaceVar l (Constant t) e) (Map.map (\q -> if		q > l
																									then	q - 1
																									else	q) $ Map.delete v m)
	Nothing		->	ParsedExpr e m
	
--	Replaces all constants that look like variables.
replaceConsts		::	ParsedExpr -> ParsedExpr
replaceConsts x	=	
	replaceVarC "V" (UniversalL) $
	replaceVarC "=" (EqualL) $
	replaceVarC "+" (PlusL) $
	replaceVarC "-" (MinusL) $
	replaceVarC "*" (TimesL) $
	replaceVarC "&" (AndL) $
	replaceVarC "and" (AndL) $
	replaceVarC "or" (OrL) $
	replaceVarC "xand" (XandL) $
	replaceVarC "xor" (XorL) $
	replaceVarC "true" (LogicL True) $
	replaceVarC "false" (LogicL False) $
	replaceVarC "T" (LogicL True) $
	replaceVarC "F" (LogicL False) $
	replaceVarC "not" (NotL) $ x
	
quickParse		::	String -> Expression
quickParse str	=	case head (parse (
							do
								possible ignoreSpace
								e	<-	expr defaultOperators
								possible ignoreSpace
								end
								return (replaceConsts e)) str) of
							(ParsedExpr x _, _)	->	x
								
-- Completely parses a file
fileParse	::	String -> IO Expression
fileParse s	=	do
						str	<-	readFile s
						return $ quickParse str
					
-- Interpreter mode					
interpret	::	String -> IO ()
interpret s	=	do
						putStrLn	"Parsing axiom file..."
						a	<-	id $! fileParse s
						putStrLn "Producing rule set..."
						--rs	<-	return $! (produceRules a)
						putStrLn	"Contemplating the existance of a higher power..."
						putStrLn "Starting interpreter..."
						conInterpret
						return ()
				where
					conInterpret	=	do
												interpretCommand
												conInterpret
						
-- Interprets a single given command
interpretCommand	:: IO ()
interpretCommand	=	do
								 putStr ">>> "
								 l	<-	getLine
								 return ()