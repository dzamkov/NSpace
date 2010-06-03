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
					tableFilter 3 notCon <^-^>
					tableMerge (tableFree) <^-^>
					tableApply 3 4 <^-^>				--	Result of notCon : 5
					tableMerge (tableFree) <^-^>
					tableMerge (tableFree) <^-^>
					tableMerge (tableFree) <^-^>
					tableFilter 6 equalCon <^-^>
					tableJoin (Set.fromList [7, 2]) <^-^>
					tableJoin (Set.fromList [8, 5]) <^-^>
					tableApply 6 7 <^-^>
					tableApply 9 8 <^-^>				--	Result of not x = notCon x : 10
					tableFilter 10 trueCon
					
					
-- Now I can use the not function
useNotFunc x 	=	case tableValue (tableFilter 1 x (notFunc)) 2 of
							Just y	->		y
							Nothing	->		undefined 	-- darn
					