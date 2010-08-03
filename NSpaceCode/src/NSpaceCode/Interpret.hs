-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Interpret
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Interpret (
	interactive
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression
import NSpaceCode.Parse
import List

-- Starts interactive interpreter mode.
interactive	::	IO ()
interactive	=	do
						putStr	">>> "
						com		<-	getLine
						processCommand com
						interactive
	where
		processCommand c	=	do
										let	parses	=	parse (do
																	e	<-	expr (listOperatorLookup defaultOperators) (listModifierLookup defaultModifiers) Nothing
																	end
																	return e) c
										case parses of
											((exprres, _):_)	->	do
																			putStrLn (show $ reduce $ varClear exprres)
											[]						->	do
																			putStrLn	"Syntax error, care to try again?"
																			
									
		--	Estimates the complexity of an expression.
		complexity	::	Expression a -> Integer
		complexity (Term _)			=	1
		complexity (Function x y)	=	complexity x + complexity y
		complexity (Lambda x)		=	complexity x * 2
		
		-- Estimates the complexity of a query
		qcomplexity	::	Query a -> Integer
		qcomplexity (Query pat tar)	=	(complexity tar `div` complexity pat)
		
		-- Computes a query given a maximum number of subquery computations. Amount of used computations are also returned.
		computeQuery	::	(Ord a) => [Rule a] -> Query a -> Integer -> ([Instance a], Integer)
		computeQuery _ _ 0				=	([], 0)
		computeQuery rules qry maxcom	=	res
			where
				qres	=	computeResult rules qry
				res	=	case qres of
								(SQueryResult qrys func)	->	case (subcompute rules qrys maxcom) of
																			(reses, ucom)	->	(func reses, ucom)
								(MQueryResult qrys func)	->	case (subcompute (abstractifyAxioms rules) qrys maxcom) of
																			(reses, ucom)	->	(func reses, ucom)
				
				-- Sorts a list of subqueries by complexity
				sortqrys	::	[Query a] -> [Query a]
				sortqrys	qrys =	sortBy (\l m -> compare (qcomplexity l) (qcomplexity m)) qrys
				
				-- Computes an amount of sorted subqueries
				subcompute	::	(Ord a) => [Rule a] -> [Query a] -> Integer -> ([[Instance a]], Integer)
				subcompute _ _ 0									=	([], 0)
				subcompute rules (qry:remainqry) maxcom	=	case (computeQuery rules qry (maxcom `div` 2)) of
																				(qryreses, ucom)	->	case (subcompute rules remainqry (maxcom - ucom - 1)) of
																					(rqryreses, nucom)	->	(qryreses:rqryreses, (ucom + nucom + 1))
				subcompute rules [] maxcom						=	([], 0)
		
		-- Reduces the complexity of an expression
		reduce	::	Expression Literal -> Expression Literal
		reduce expr	=	res
			where
				axioms	=	lambdaAxioms ++ literalAxioms
				comqry	=	computeQuery axioms (Query (Term $ Left $ 0) expr) 1000
				exprres	=	expr:(map (\l -> Map.fold (\it ac -> it) expr l) $ fst comqry)
				res		=	minimumBy (\l m -> compare (complexity l) (complexity m)) exprres
				
				