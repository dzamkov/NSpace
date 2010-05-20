-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Type
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Type (
	Value(..),
	ValueType,
	valueAnd,
	valueOr,
	valueEq,
	valueITE,
	valueNot
) where 

import NSpaceCode.Expression

data Value		=	
	BoolValue Bool deriving (Eq, Show)
	
type ValueType		=	SimpleType Value

arg1Con			::	(ValueType -> ValueType) -> ValueType
arg1Con x		=	TypeFunction x

arg2Con			::	(ValueType -> ValueType -> ValueType) -> ValueType
arg2Con x		=	TypeFunction (\a -> TypeFunction (\b -> x a b))

arg3Con			::	(ValueType -> ValueType -> ValueType -> ValueType) -> ValueType
arg3Con x		=	TypeFunction (\a -> TypeFunction (\b -> TypeFunction (\c -> x a b c)))

baseNot																			:: ValueType -> ValueType
baseNot (TypeValue (BoolValue x))										=	TypeValue (BoolValue (not x))

baseAnd																			::	ValueType -> ValueType -> ValueType
baseAnd (TypeValue (BoolValue x)) (TypeValue (BoolValue y))		= 	TypeValue (BoolValue (x && y))
baseAnd x y																		=	Undefined

baseOr																			::	ValueType -> ValueType -> ValueType
baseOr (TypeValue (BoolValue x)) (TypeValue (BoolValue y))		= 	TypeValue (BoolValue (x || y))
baseOr x y																		=	Undefined

baseEq																			::	ValueType -> ValueType -> ValueType
baseEq x y																		=	TypeValue (BoolValue (x == y))

baseITE																			:: ValueType -> ValueType -> ValueType -> ValueType
baseITE (TypeValue (BoolValue x)) y z									=	if x then y else z

valueNot	=	arg1Con baseNot
valueAnd	=	arg2Con baseAnd
valueOr	=	arg2Con baseOr
valueEq	=	arg2Con baseEq
valueITE	=	arg3Con baseITE