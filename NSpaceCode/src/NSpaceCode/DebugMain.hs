-----------------------------------------------------------------------------
--
-- Module      :  DebugMain
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

import qualified Data.Set as Set
import NSpaceCode.Expression

-- Test exp:	forall a, b, c (a + b = c) = (a = c - b) and x + 7 = 3 + 9
var0	=	createVar 0 	-- a
var1	=	createVar 1 	-- b
var2	=	createVar 2 	-- c
var3	=	createVar 3 	-- =
var4	=	createVar 4 	-- +
var5	=	createVar 5		-- and
var6	=	createVar 6 	-- x
var7	=	createVar 7 	-- 7
var8	=	createVar 8 	-- 3
var9	=	createVar 9		-- 9
var10	=	createVar 10	-- -
exp0	=	createFunc (createFunc var4 var0) var1			-- a + b
exp1	=	createFunc (createFunc var3 exp0) var2			-- a + b = c
exp2	=	createFunc (createFunc var10 var2) var1		-- c - b
exp3	=	createFunc (createFunc var3 var0) exp2			-- a = c - b
exp4	=	createFunc (createFunc var3 exp1) exp3			-- (a + b = c) = (a = c - b)
exp5	=	createForAll (Set.fromList [0, 1, 2]) exp4	-- forall a, b, c (a + b = c) = (a = c - b)
exp6	=	createFunc (createFunc var4 var6) var7			-- x + 7
exp7	=	createFunc (createFunc var4 var8) var9			-- 3 + 9
exp8	=	createFunc (createFunc var3 exp6) exp7			-- x + 7 = 3 + 9
exp9	=	createFunc (createFunc var5 exp5) exp8			-- forall a, b, c (a + b = c) = (a = c - b) and x + 7 = 3 + 9 