-----------------------------------------------------------------------------
--
-- Module      :  DebugMain
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

import NSpaceCode.Expression
import NSpaceCode.Type

bool2Exp x	=	Constant $ TypeValue $ BoolValue $ x

testExp1		=	(Function (Constant valueNot) (bool2Exp True))
testExp2 	=	(Function (Function (Function (Constant valueITE) (bool2Exp True)) (bool2Exp False)) (bool2Exp True))
