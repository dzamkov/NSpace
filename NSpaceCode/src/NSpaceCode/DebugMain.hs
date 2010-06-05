-----------------------------------------------------------------------------
--
-- Module      :  DebugMain
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import NSpaceCode.Expression
import NSpaceCode.Value

-- Happy operator
infixl 9 <^-^>
(<^-^>)			::	a -> (a -> a) -> a
(<^-^>) x y		=	y x

-- Logical not function
notFunc		=	(tableEmpty :: SimpleTable String) <^-^>
					tableMerge (tableFree) <^-^>
					tableMerge (tableFree) <^-^>
					tableApply 0 1 <^-^>				-- Result of not: 2
					tableMerge (tableFree) <^-^>
					tableFilter 3 (constantValue notCon) <^-^>
					tableMerge (tableFree) <^-^>
					tableApply 3 4 <^-^>				--	Result of notCon : 5
					tableMerge (tableFree) <^-^>
					tableMerge (tableFree) <^-^>
					tableMerge (tableFree) <^-^>
					tableFilter 6 (constantValue equalCon) <^-^>
					tableJoin (Set.fromList [7, 2]) <^-^>
					tableJoin (Set.fromList [8, 5]) <^-^>
					tableApply 6 7 <^-^>
					tableApply 9 8 <^-^>				--	Result of not x = notCon x : 10
					tableFilter 10 (constantValue trueCon)
					
					
-- Now I can use the not function
useNotFunc x 	=	case tableValue (tableFilter 1 (constantValue x) (notFunc)) 2 of
							Just y	->		y
							Nothing	->		undefined 	-- darn
					
-- Test of eq and functions in general (= x (=x) y (x=y))
testTab	=	(tableEmpty :: SimpleTable String) <^-^>
				tableMerge (tableFree) <^-^>
				tableFilter 0 (constantValue equalCon) <^-^>
				tableMerge (tableFree) <^-^>
				tableApply 0 1 <^-^>
				tableMerge (tableFree) <^-^>
				tableApply 2 3 <^-^>
				tableFilter 4 (constantValue trueCon)
				
valTab	=	tableFilter 1 (constantValue "rofl") (testTab)
val		=	tableValue valTab 3

-- Test appmap
appTab	=	simplify ((tableEmpty :: SimpleTable String) <^-^>
				tableMerge (tableFree) <^-^>
				tableFilter 0 (constantValue notCon) <^-^>
				tableMerge (tableFree) <^-^>
				tableFilter 1 (constantValue trueCon) <^-^>
				tableApply 0 1)
