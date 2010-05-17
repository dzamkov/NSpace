-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Vector
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Vector (
	Vector(..),
	vecFromList,
	addVec,
	subVec,
	multVecScl,
	toVertex3
) where

import Graphics.Rendering.OpenGL

-- A vector represents a point in a local three dimensional coordinate space.

data Vector		=	Vector {
							getX	::	Double,
							getY	::	Double,
							getZ	::	Double } deriving(Eq, Show)

vecFromList					::	[Double] -> Vector
vecFromList (x:y:z:s)	=	Vector x y z
vecFromList a				=	undefined
							
addVec		::	Vector -> Vector -> Vector
addVec a b	=	Vector (getX a + getX b) (getY a + getY b) (getZ a + getZ b) 

subVec		::	Vector -> Vector -> Vector
subVec a b	=	Vector (getX a - getX b) (getY a - getY b) (getZ a - getZ b)

multVecScl			::	Vector -> Double -> Vector
multVecScl a b		=	Vector (getX a * b) (getY a * b) (getZ a * b)

toVertex3		::	Vector -> Vertex3 GLdouble
toVertex3 a		=	Vertex3 (getX a) (getY a) (getZ a)