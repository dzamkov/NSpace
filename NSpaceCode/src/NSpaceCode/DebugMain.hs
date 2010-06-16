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
		(ApplyTable
			(ApplyTable
				(MergeTable
					(MergeTable
						(MergeTable
							(MiniTable $ ConsValue $ AndCons)
							(FreeTable))
						(FreeTable))
					(MiniTable $ ConsValue $ LogicCons True))
				0 1)
			4 2)
		5 3))
		
testval	=	tableValue testtab 1