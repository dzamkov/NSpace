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
import NSpaceCode.Parse

-- Happy operator
infixl 9 <^-^>
(<^-^>)			::	a -> (a -> a) -> a
(<^-^>) x y		=	y x

-- Constant type
data Constant	=
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
	
instance Cons Constant where
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
	
-- Parsinate some test text
parsinate	=	do
	contents		<-		readFile "../ns/test.ns"
	return (parse contents)