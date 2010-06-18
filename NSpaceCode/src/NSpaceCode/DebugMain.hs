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
		
myimplies	=	implies testexp 1
testval		=	myimplies $ myimplies (Set.singleton $ Variable 0)