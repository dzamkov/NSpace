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
import qualified List as List
import NSpaceCode.Expression

-- Parses a string and returns valid matches paired with a suffix string

newtype Parser a	=	Parser (String -> [(a, String)])

parse (Parser a)	=	a

instance Monad Parser where
	return a	=	Parser (\cs -> [(a, cs)])
	p >>= f	=	Parser (\cs -> concat [parse (f a) cs' |
						(a, cs') <- parse p cs])
						
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
						
listLiteral				::	Char -> Char -> Operators -> Parser [Expression String]
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

type Term	=	Either (Expression String) String
	
expr	::	Operators  -> Parser (Expression String)
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
							(Just l)	->	return $ Function (Function (Function (Constant $ ITEL) cond) act) l 
							Nothing	->	return $ Function (Function (Function (Constant $ ITEL) cond) act) (Constant $ LogicL $ True)),
					modifierTest "forall" Forall,
					modifierTest "exists" Exists,
					modifierTest "lambda" Lambda,
					modifierTest "solve" Solve]
			where
				modifierTest			::	String -> ModifierType -> Parser (Expression String)
				modifierTest s mod	=	do
												vars	<-	modifier s
												possible ignoreSpace
												e		<-	expr ops
												return $ foldr (\var ac ->	(Modifier mod ac var)) e vars
												
			
				termParse	::	Parser [Term]
				termParse	=	do
										term	<-	union [
											(do
												r	<-	word
												case operatorInfo r ops of
													Just (str, asc)	->	return (Right r)
													Nothing				->	return (Left $ Variable r)),
											(do
												char '('
												possible ignoreSpace
												e	<-	expr ops
												possible ignoreSpace
												char ')'
												return (Left e)),
											(do
												str	<-	stringLiteral
												return (Left $
													foldl (\c l -> 
														Function c (Constant $ CharL l)
													) (Constant $ ListL) str)),
											(do
												int	<-	intLiteral
												return (Left $ Constant $ IntegerL int)),
											(do
												li		<-	listLiteral '[' ']' ops
												return (Left $ foldl (\c l -> Function c l) 
													(Constant ListL) li)),
											(do
												li		<-	listLiteral '{' '}' ops
												return (Left $ foldl (\c l -> Function c l) 
													(Constant SetL) li))]
										rs	<-	conParse
										return (term:rs)
				conParse		::	Parser [Term]
				conParse		=	do
										w	<-	possible ignoreSpace
										case w of
											(Just _)	->	termParse
											Nothing	->	return []
				termReduce		::	[Term] -> Maybe (Expression String)
				termReduce x	=	case operatorReduce $ functionReduce x of
											[Left l]				->	Just l
											_						->	Nothing
					where
						functionReduce	((Left x):(Left y):cs)	=	functionReduce $ (Left $ Function x y):cs	
						functionReduce (x:rs)						=	x:(functionReduce rs)
						functionReduce []								=	[]
						
						operatorReduce		::	[Term] -> [Term]
						operatorReduce x	=	foldl (\cur op -> case op of
														(LeftAssociative, opset)	->	laOpReduce cur opset) x ops
							where
								laOpReduce	::	[Term] -> Set.Set String -> [Term]
								laOpReduce ((Left f):(Right op):(Left a):rs) opset
									|	Set.member op opset	=	laOpReduce ((Left 
																			(Function
																				(Function
																					(Variable op)
																					f)
																				a)):rs) opset
								laOpReduce (x:rs) opset	=	x:(laOpReduce rs opset)
								laOpReduce [] _			=	[]
								
-- Replaces a variable in an expression with a constant
replaceVarC	::	String -> Literal -> Expression String -> Expression String
replaceVarC v t (Variable s)
	|	s == v							=	Constant t
	|	otherwise						=	Variable s
replaceVarC v t (Function a b)	=	Function (replaceVarC v t a) (replaceVarC v t b)
replaceVarC v t (Modifier m e p)
	|	p == v							=	Modifier m e p
	|	otherwise						=	Modifier m (replaceVarC v t e) p
replaceVarC _ _ (Constant c)		=	Constant c
	
-- Mapping of constants and stringhs
consts	=	Map.fromList [
	("V", UniversalL),
	("=", EqualL),
	("+", PlusL),
	("-", MinusL),
	("*", TimesL),
	("&", AndL),
	("and", AndL),
	("or", OrL),
	("xand", XandL),
	("xor", XorL),
	("true", LogicL True),
	("false", LogicL False),
	("T", LogicL True),
	("F", LogicL False),
	("not", NotL)]
	
rconsts	=	Map.fromList [
	(UniversalL, "V"),
	(EqualL, "="),
	(PlusL, "+"),
	(MinusL, "-"),
	(TimesL, "*"),
	(AndL, "and"),
	(OrL, "or"),
	(XandL, "xand"),
	(XorL, "xor"),
	(LogicL True, "true"),
	(LogicL False, "false"),
	(NotL, "not")]
	
--	Replaces all constants that look like variables.
replaceConsts		::	Expression String -> Expression String
replaceConsts x	=	Map.foldWithKey (\k v a -> replaceVarC k v a) x consts
	
quickParse		::	String -> Expression String
quickParse str	=	fst $ head (parse (
							do
								possible ignoreSpace
								e	<-	expr defaultOperators
								possible ignoreSpace
								end
								return (replaceConsts e)) str)
							
-- Completely parses a file
fileParse	::	String -> IO (Expression String)
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