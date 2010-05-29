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

testExp			=	funcDefinite (varDefinite "lol") (varDefinite "wtf")
abstractExp		=	funcDefinite (abstractDefinite) (abstractDefinite)
selfApplyTestExp		=	foldDefinite (funcDefinite (varDefinite "rofl") (varDefinite "rofl")) (Set.fromList [0, 1])
selfApplyAbstractExp	=	foldDefinite (funcDefinite (abstractDefinite) (abstractDefinite)) (Set.fromList [0, 1])
complexTestExp	=	foldDefinite (funcDefinite (funcDefinite (varDefinite "rofl") (varDefinite "omg")) (varDefinite "rofl")) (Set.fromList [0, 2])