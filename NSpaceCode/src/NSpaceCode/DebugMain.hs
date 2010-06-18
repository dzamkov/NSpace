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
				
while test action = do
	val <- test
	if 	val 
		then 	(do 
			action
			while test action)
		else return ()
	
runProcess	::	(Cons a) => Expression a -> Int -> IO ()
runProcess s c	=	do
	curset	<-	newIORef (Set.singleton $ testexp)
	while (return True) (do
		val	<-	readIORef curset
		nval	<- return (process val c)
		writeIORef curset nval
		putStrLn ("Current statements processing: " ++ (show (Set.size nval))))
		
main	=	runProcess testexp 1