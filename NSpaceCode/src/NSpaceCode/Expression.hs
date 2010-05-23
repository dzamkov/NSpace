-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression,
	createVar,
	createFunc,
	createForAll,
	SpecialVarSet(..),
	TruthSet(..),
	createProgram
) where 

import Data.Unique
import qualified Data.Set as Set
import qualified Data.Map as Map

-- A relation between variables, and functions
-- that produces a definite value. The provided type to
-- Expression determines the type of the value it can 
-- evaluate to.

data Expression		=	
	Variable Int |
	Function Expression Expression |
	ForAll [Int] Expression
	
-- Creates a new variable from a variable index.
	
createVar		:: Int -> Expression
createVar x		=	Variable x

-- Creates a function as the application of an expression on
-- another. There should be a direct relation of variables between
-- the two expression.

createFunc			::	Expression -> Expression -> Expression
createFunc x y		=	Function x y

-- Creates a special forall expression which is true if and only if the
-- inner expression is true for all possible values of the specified variables.
											
createForAll			::	[Int] -> Expression -> Expression
createForAll x y		=	ForAll x y
												
-- Programs are defined by a special variable set and a true expression. A truth
-- set contains expressions that are all true.
							
data SpecialVarSet	=	SpecialVarSet {
	getTrueVar		::	Int,
	getFalseVar		::	Int,
	getEqualsVar	::	Int,
	getAndVar		::	Int,
	getIFTVar		::	Int } deriving (Show, Eq)
	
data TruthSet	=	TruthSet {
	getTrueExpressions	::	Set.Set Expression,
	getSpecialVarSet	::	SpecialVarSet }
	
-- Creates a program from a true expression and special var set.
	
createProgram		::	Expression -> SpecialVarSet -> TruthSet
createProgram x y	=	TruthSet (Set.singleton x) y
