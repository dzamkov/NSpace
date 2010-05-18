-----------------------------------------------------------------------------
--
-- Module      :  NSpace.CanBe
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.CanBe (
	CanBe(..)
) where

-- Indicates that a specified class can also be another, but doesn't have to. This
-- means that B (the casted type) is a type of A (the base type)

class CanBe a b where
	cast		::	a -> Maybe b