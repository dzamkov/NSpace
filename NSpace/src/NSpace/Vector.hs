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
	divVecScl,
	multVecScl,
	crossVec,
	lengthVec,
	normVec,
	zeroVec,
	xVec,
	yVec,
	zVec,
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

divVecScl		::	Vector -> Double -> Vector
divVecScl a b	=	Vector (getX a / b) (getY a / b) (getZ a / b)

lengthVec		::	Vector -> Double
lengthVec a		=	sqrt ((getX a) * (getX a) + (getY a) * (getY a) + (getZ a) * (getZ a))

normVec		::	Vector -> Vector
normVec a	=	divVecScl a $ lengthVec a

crossVec			::	Vector -> Vector -> Vector
crossVec a b	=	Vector x y z 
					where
						x	=	(getY a * getZ b) - (getZ a * getY b)
						y	=	(getZ a * getX b) - (getX a * getZ b)
						z	=	(getX a * getY b) - (getY a * getX b)

multVecScl			::	Vector -> Double -> Vector
multVecScl a b		=	Vector (getX a * b) (getY a * b) (getZ a * b)

zeroVec		::	Vector
zeroVec		=	Vector 0 0 0

xVec		::	Vector
xVec		=	Vector 1 0 0

yVec		::	Vector
yVec		=	Vector 0 1 0

zVec		::	Vector
zVec		=	Vector 0 0 1

toVertex3		::	Vector -> Vertex3 GLdouble
toVertex3 a		=	Vertex3 (getX a) (getY a) (getZ a)