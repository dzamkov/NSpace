-----------------------------------------------------------------------------
--
-- Module      :  DebugMain
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set
import NSpaceCode.Value

testtab	=	(
	(JoinTable
		(MergeTable 
			(FreeTable)
			(MiniTable $ ConsValue $ IntegerCons 6))
		0 1))
		
testval	=	tableValue testtab 0