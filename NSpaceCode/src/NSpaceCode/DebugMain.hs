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
			(Constant EqualCons)
			(Variable 0))
		(Constant (IntegerCons 5))))
		
testval	=	implies testexp 1 (Variable 0)