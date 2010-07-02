-----------------------------------------------------------------------------
--
-- Module      :  DebugMain
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set
import NSpaceCode.Expression
import NSpaceCode.Value
import NSpaceCode.Parse
import Data.IORef

-- x = 5

testexp 	=	(
	(Function
		(Function
			(Constant AndCons)
			(Function
				(Function
					(Constant EqualCons)
					(Variable 0))
				(Constant (IntegerCons 5))))
		(Function
			(Function
				(Constant EqualCons)
				(Constant (IntegerCons 5)))
			(Function
				(Function 
					(Constant PlusCons)
					(Constant (IntegerCons 3)))
				(Constant (IntegerCons 2))))))
				
main	=	solve (initSolver (Variable 0) testexp) (\s -> do
	putStrLn "--------"
	putStrLn ("Current Open Targets: " ++ (show $ Set.size $ openTargetExps s))
	putStrLn (show (openTargetExps s))
	putStrLn ("Current Closed Targets: " ++ (show $ Set.size $ closedTargetExps s))
	putStrLn (show (closedTargetExps s))
	putStrLn ("Current Statements: " ++ (show $ Set.size $ statementExps s))
	putStrLn (show (statementExps s))
	getLine
	return True)
	
parseAxioms	::	IO (Expression SimpleCons)
parseAxioms	=	fileParse "../ns/axiom.ns"
