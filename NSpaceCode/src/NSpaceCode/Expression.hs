-----------------------------------------------------------------------------
--
-- Module      :  NSpaceCode.Expression
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpaceCode.Expression (
	Expression(..)
) where 


-- A relation between variables, and functions
-- that produces a definite value. The provided type to
-- Expression determines the type of the value it can 
-- evaluate to.

data Expression		=	
	Variable Int |
	Function Expression Expression