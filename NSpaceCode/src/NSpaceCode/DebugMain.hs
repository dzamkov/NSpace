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

-- Constant type
data MyConstant	=
	ConsInteger Integer |
	ConsString String |
	ConsEqual |
	ConsITE |
	ConsAnd |
	ConsOr |
	ConsXor |
	ConsNot |
	ConsTrue |
	ConsFalse |
	ConsPlus |
	ConsMinus |
	ConsMult deriving (Show, Ord, Eq)
	
instance Cons MyConstant where
	equalCon 	= 	ConsEqual
	iteCon		=	ConsITE
	andCon		=	ConsAnd
	orCon			=	ConsOr
	xorCon		=	ConsXor
	notCon		=	ConsNot
	trueCon		=	ConsTrue
	falseCon		=	ConsFalse
	
	eval (Branch (Branch (Leaf ConsPlus) (Leaf (ConsInteger x))) (Leaf (ConsInteger y)))	=	Just (ConsInteger (x + y))
	eval (Branch (Branch (Leaf ConsMinus) (Leaf (ConsInteger x))) (Leaf (ConsInteger y)))	=	Just (ConsInteger (x - y))
	eval (Branch (Branch (Leaf ConsMult) (Leaf (ConsInteger x))) (Leaf (ConsInteger y)))	=	Just (ConsInteger (x * y))
	eval _																											=	Nothing
	
myexp		=	Function (Function (Constant ConsEqual) (Variable "x")) (Constant $ ConsInteger 5)
mytab		=	(expToTable myexp) :: ExpressionTable (SimpleTable MyConstant)
mysol		=	solve myexp "x"
myntab	=	case mysol of (Just (ColumnValue x _)) -> x