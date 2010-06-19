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
		
setImplies c t s	=	Set.fold (\x y -> Set.union y (implies c x s)) Set.empty t
	
runImplies	::	(Cons a) => Int -> Expression a -> Expression a -> IO ()
runImplies c t s	=	do
	curset	<-	newIORef (Set.singleton $ t)
	while (return True) (do
		val	<-	readIORef curset
		nval	<- return (setImplies c val s)
		writeIORef curset nval
		putStrLn ("Current statements processing: " ++ (show (Set.size nval))))
		
main	=	runImplies 1 (Variable 0) testexp