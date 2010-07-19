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

(^-^)			::	String -> [String] -> Pattern String
(^-^) p l	=	patternize (quickParse p) l

--	Set of axiomatic rules for expressions.

axioms			=	[
		Rule 
			(Constant $ LogicL True)
			("solve x x = y"		^-^	["y"])
			("{y}"					^-^	["y"])
	]