-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Interpret
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Interpret (
	interactive
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression
import NSpaceCode.Parse

-- Starts interactive interpreter mode.
interactive	::	IO ()
interactive	=	do
						putStr	">>> "
						com		<-	getLine
						interactive