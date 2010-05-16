-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Vector
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Vector (
	Vector(..),
	addVec,
	subVec,
	toVertex3
) where

import Graphics.Rendering.OpenGL

-- A vector represents a point in a local three dimensional coordinate space.

data Vector		=	Vector {
							getX	::	Double,
							getY	::	Double,
							getZ	::	Double } deriving(Eq, Show)

addVec		::	Vector -> Vector -> Vector
addVec a b	=	Vector (getX a + getX b) (getY a + getY b) (getZ a + getZ b) 

subVec		::	Vector -> Vector -> Vector
subVec a b	=	Vector (getX a - getX b) (getY a - getY b) (getZ a - getZ b)

toVertex3		::	Vector -> Vertex3 GLdouble
toVertex3 a		=	Vertex3 (getX a) (getY a) (getZ a)