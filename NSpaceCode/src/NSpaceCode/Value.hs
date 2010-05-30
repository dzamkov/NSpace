-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Value (
	LogicOperationSet(..),
	Statement(..),
	getConclusion,
	Value(..),
	createTrueValue
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import NSpaceCode.Expression

-- Contains a map of known logic operations for a specified type.

data LogicOperationSet a 	=	LogicOperationSet {
	equalOp	::	a,
	andOp		::	a,
	orOp		::	a,
	xorOp		::	a,
	iteOp		::	a,
	trueOp	::	a,
	falseOp	::	a}
	
-- An expression known to be true in a system.
	
data Statement a	=
	Axiom (Definite a) |
	Deduction (Definite a) (Statement a) |
	Substitution (Definite a) (Statement a) (Statement a)
	
-- Gets the conclusion part of a statement, which is a true definite expression

getConclusion								::	Statement a -> Definite a
getConclusion (Axiom x)					=	x
getConclusion (Deduction x _)			=	x
getConclusion (Substitution x _ _)	=	x

-- A value represented by an infinite set of expressions that are equivelant to
-- each other. B is the type of variables in the expressions for the value. 

class Value a b where 
	matchValue			::	a -> Definite b -> Int -> a	-- Given a pattern, this finds a value for the specified abstract variable in the pattenr.
	singleVariable		::	a -> Maybe b						-- Tries getting a single variable for the value
	
-- Creates a value from a true definite expression.
	
createTrueValue		::	Definite a -> LogicOperationSet a -> StatementValue a
createTrueValue x y	=	StatementValue (StatementSet (Set.empty) (Set.singleton $ Axiom x) y) (varDefinite $ trueOp y)

-- Set of statements and logical rules governing them

data StatementSet a	=	StatementSet {
	getClosedSet	::	Set.Set (Statement a),		--	Fully processed statements
	getOpenSet		::	Set.Set (Statement a), 		-- Statements, whose derived statements are still unknown
	getLogic			:: LogicOperationSet a }		-- Logical operations
	
-- Value made by statements
	
data StatementValue a	=	StatementValue {
	getStatementSet		::	StatementSet a,	-- Statements about system
	getValueExpression	::	Definite a }		-- Expression of the value in system
	