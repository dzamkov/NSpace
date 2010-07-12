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
--import NSpaceCode.Parse
import Data.IORef

--main	=	interpret "../ns/axiom.ns"

texp		=	Function Variable Variable Set.empty
nexp		=	Function texp texp (Set.fromList [(0, 0)])