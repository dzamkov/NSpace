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
	(ApplyTable
		(ApplyTable
			(MergeTable
				(MergeTable
					(MiniTable $ ConsValue $ PlusCons)
					(MiniTable $ ConsValue $ IntegerCons 17))
				(MiniTable $ ConsValue $ IntegerCons 24))
			0 1)
		3 2))
		
testval	=	tableValue testtab 4