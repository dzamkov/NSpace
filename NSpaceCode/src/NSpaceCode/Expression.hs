-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..),
	Type(..),
	SimpleType(..),
	evaluateExp
) where 


-- A relation between variables, constants and functions
-- that produces a definite value. The provided type to
-- Expression determines the type of the value it can 
-- evaluate to.

data Expression a		=	
	Constant a |
	Variable Int |
	Function (Expression a) (Expression a)
	
class (Eq a) => Type a where
	undefinedV	::	a
	apply			::	a -> a -> a
	
-- A type that can either be a value, a function or undefined.
	
data SimpleType a		=
	TypeValue a |
	TypeFunction ((SimpleType a) -> (SimpleType a)) |
	Undefined
	
instance (Show a) => Show (SimpleType a) where
	show (TypeValue x)		=	show x
	show (TypeFunction x)	=	"function"
	show (Undefined)			=	"undefined"
	
instance (Eq a) => Eq (SimpleType a) where
	(TypeValue x) == (TypeValue y)		=	x == y
	(Undefined) == (Undefined)				=	True
	x == y										=	False
	
instance (Eq a) => Type (SimpleType a) where
	undefinedV										=	Undefined
	apply (TypeFunction x) y					=	x y
	apply x y										=	Undefined
	
-- Evaluates an expression given a starting expression and a variable to value
-- map.

evaluateExp							::	(Type a) => Expression a -> (Int -> a) -> a
evaluateExp (Constant x) y		=	x
evaluateExp	(Variable x) y		=	y x
evaluateExp (Function x y) z	=	apply (evaluateExp x z) (evaluateExp y z)