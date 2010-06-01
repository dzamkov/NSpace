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

true	=	tableFilterValue (tableFree :: SimpleTable String) 0 "true"