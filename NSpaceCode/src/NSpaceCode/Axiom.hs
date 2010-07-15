-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Axiom
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Axiom (
	axioms
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression
import NSpaceCode.Parse

--	Set of axiomatic rules for expressions.

axioms			=	Set.fromList [
	Rule			
		(Constant $ LogicL True)			
		(quickParse "solve x x = y")			
		(quickParse "y")			
		(Set.fromList [(Nothing, Just 0, Just 0)])
]