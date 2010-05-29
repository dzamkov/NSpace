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

testExp			=	Definite (Function Variable Variable) (Map.fromList [(0, "lol"), (1, "omg")])
abstractExp		=	Definite (Function Variable Variable) (Map.empty)