-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Matrix
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Matrix (
	Matrix(..),
	multMat,
	transMat,
	transMatVec,
	invMat
) where

import qualified Graphics.Rendering.OpenGL as GL
import NSpace.Event
import NSpace.Vector
import NSpace.ReferenceFrame

-- A matrix representing an affline transform appliable to vectors.
-- [X.X	Y.X	Z.X	T.X]
-- [X.Y	Y.Y	Z.Y	T.Y]
-- [X.Z	Y.Z	Z.Z	T.Z]
-- [0		0		0		1  ]

data Matrix		=	Matrix {
							getM11 :: Double,
							getM12 :: Double,
							getM13 :: Double,
							getM14 :: Double,
							getM21 :: Double,
							getM22 :: Double,
							getM23 :: Double,
							getM24 :: Double,
							getM31 :: Double,
							getM32 :: Double,
							getM33 :: Double,
							getM34 :: Double,
							getM41 :: Double,
							getM42 :: Double,
							getM43 :: Double,
							getM44 :: Double } deriving(Eq, Show)
						
transMatVec			::	Matrix -> Vector -> Vector
transMatVec a b		=	Vector nx ny nz
						where 
							nx		=	((getM11 a) * (getX b)) + ((getM12 a) * (getY b)) + ((getM13 a) * (getZ b)) + getM14 a
							ny		=	((getM21 a) * (getX b)) + ((getM22 a) * (getY b)) + ((getM23 a) * (getZ b)) + getM24 a
							nz		=	((getM31 a) * (getX b)) + ((getM32 a) * (getY b)) + ((getM33 a) * (getZ b)) + getM34 a
							
multMat			::	Matrix -> Matrix -> Matrix
multMat a b		= Matrix m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m43 m41 m42 m43 m44
					where
						m11 	=	((getM11 a) * (getM11 b)) + ((getM12 a) * (getM21 b)) + ((getM13 a) * (getM31 b)) + ((getM14 a) * (getM41 b))
						m12	=	((getM11 a) * (getM12 b)) + ((getM12 a) * (getM22 b)) + ((getM13 a) * (getM32 b)) + ((getM14 a) * (getM42 b))
						m13	=	((getM11 a) * (getM13 b)) + ((getM12 a) * (getM23 b)) + ((getM13 a) * (getM33 b)) + ((getM14 a) * (getM43 b))
						m14	=	((getM11 a) * (getM14 b)) + ((getM12 a) * (getM24 b)) + ((getM13 a) * (getM34 b)) + ((getM14 a) * (getM44 b))

						m21	=	((getM21 b) * (getM11 b)) + ((getM22 b) * (getM21 b)) + ((getM23 b) * (getM31 b)) + ((getM24 b) * (getM41 b))
						m22	=	((getM21 b) * (getM12 b)) + ((getM22 b) * (getM22 b)) + ((getM23 b) * (getM32 b)) + ((getM24 b) * (getM42 b))
						m23	=	((getM21 b) * (getM13 b)) + ((getM22 b) * (getM23 b)) + ((getM23 b) * (getM33 b)) + ((getM24 b) * (getM43 b))
						m24	=	((getM21 b) * (getM14 b)) + ((getM22 b) * (getM24 b)) + ((getM23 b) * (getM34 b)) + ((getM24 b) * (getM44 b))

						m31	=	((getM31 b) * (getM11 b)) + ((getM32 b) * (getM21 b)) + ((getM33 b) * (getM31 b)) + ((getM34 b) * (getM41 b))
						m32	=	((getM31 b) * (getM12 b)) + ((getM32 b) * (getM22 b)) + ((getM33 b) * (getM32 b)) + ((getM34 b) * (getM42 b))
						m33	=	((getM31 b) * (getM13 b)) + ((getM32 b) * (getM23 b)) + ((getM33 b) * (getM33 b)) + ((getM34 b) * (getM43 b))
						m34	=	((getM31 b) * (getM14 b)) + ((getM32 b) * (getM24 b)) + ((getM33 b) * (getM34 b)) + ((getM34 b) * (getM44 b))
						
						m41	=	((getM41 b) * (getM11 b)) + ((getM42 b) * (getM21 b)) + ((getM43 b) * (getM31 b)) + ((getM44 b) * (getM41 b))
						m42	=	((getM41 b) * (getM12 b)) + ((getM42 b) * (getM22 b)) + ((getM43 b) * (getM32 b)) + ((getM44 b) * (getM42 b))
						m43	=	((getM41 b) * (getM13 b)) + ((getM42 b) * (getM23 b)) + ((getM43 b) * (getM33 b)) + ((getM44 b) * (getM43 b))
						m44	=	((getM41 b) * (getM14 b)) + ((getM42 b) * (getM24 b)) + ((getM43 b) * (getM34 b)) + ((getM44 b) * (getM44 b))
						
transMat			:: Matrix -> Matrix -> Matrix
transMat			=	multMat
							
invMat			::	Matrix -> Matrix
invMat a			= Matrix nm11 nm12 nm13 nm14 nm21 nm22 nm23 nm24 nm31 nm32 nm33 nm43 nm41 nm42 nm43 nm44
					where
						m11	=	((getM22 a) * (getM33 a) * (getM44 a)) + ((getM23 a) * (getM34 a) * (getM42 a)) + ((getM24 a) * (getM32 a) * (getM43 a)) -
							 ((getM22 a) * (getM34 a) * (getM43 a)) - ((getM23 a) * (getM32 a) * (getM44 a)) - ((getM24 a) * (getM33 a) * (getM42 a))
						m12	=	((getM12 a) * (getM34 a) * (getM43 a)) + ((getM13 a) * (getM32 a) * (getM44 a)) + ((getM14 a) * (getM33 a) * (getM42 a)) -
							 ((getM12 a) * (getM33 a) * (getM44 a)) - ((getM13 a) * (getM34 a) * (getM42 a)) - ((getM14 a) * (getM32 a) * (getM43 a))
						m13	=	((getM12 a) * (getM23 a) * (getM44 a)) + ((getM13 a) * (getM24 a) * (getM42 a)) + ((getM14 a) * (getM22 a) * (getM43 a)) -
							 ((getM12 a) * (getM24 a) * (getM43 a)) - ((getM13 a) * (getM22 a) * (getM44 a)) - ((getM14 a) * (getM23 a) * (getM42 a))
						m14	=	((getM12 a) * (getM24 a) * (getM33 a)) + ((getM13 a) * (getM22 a) * (getM34 a)) + ((getM14 a) * (getM23 a) * (getM32 a)) -
							 ((getM12 a) * (getM23 a) * (getM34 a)) - ((getM13 a) * (getM24 a) * (getM32 a)) - ((getM14 a) * (getM22 a) * (getM33 a))

						m21	=	((getM21 a) * (getM34 a) * (getM43 a)) + ((getM23 a) * (getM31 a) * (getM44 a)) + ((getM24 a) * (getM33 a) * (getM41 a)) -
							 ((getM21 a) * (getM33 a) * (getM44 a)) - ((getM23 a) * (getM34 a) * (getM41 a)) - ((getM24 a) * (getM31 a) * (getM43 a))
						m22	=	((getM11 a) * (getM33 a) * (getM44 a)) + ((getM13 a) * (getM34 a) * (getM41 a)) + ((getM14 a) * (getM31 a) * (getM43 a)) -
							 ((getM11 a) * (getM34 a) * (getM43 a)) - ((getM13 a) * (getM31 a) * (getM44 a)) - ((getM14 a) * (getM33 a) * (getM41 a))
						m23	=	((getM11 a) * (getM24 a) * (getM43 a)) + ((getM13 a) * (getM21 a) * (getM44 a)) + ((getM14 a) * (getM23 a) * (getM41 a)) -
							 ((getM11 a) * (getM23 a) * (getM44 a)) - ((getM13 a) * (getM24 a) * (getM41 a)) - ((getM14 a) * (getM21 a) * (getM43 a))
						m24	=	((getM11 a) * (getM23 a) * (getM34 a)) + ((getM13 a) * (getM24 a) * (getM31 a)) + ((getM14 a) * (getM21 a) * (getM33 a)) -
							 ((getM11 a) * (getM24 a) * (getM33 a)) - ((getM13 a) * (getM21 a) * (getM34 a)) - ((getM14 a) * (getM23 a) * (getM31 a))

						m31	=	((getM21 a) * (getM32 a) * (getM44 a)) + ((getM22 a) * (getM34 a) * (getM41 a)) + ((getM24 a) * (getM31 a) * (getM42 a)) -
							((getM21 a) * (getM34 a) * (getM42 a)) - ((getM22 a) * (getM31 a) * (getM44 a)) - ((getM24 a) * (getM32 a) * (getM41 a))
						m32	=	((getM11 a) * (getM34 a) * (getM42 a)) + ((getM12 a) * (getM31 a) * (getM44 a)) + ((getM14 a) * (getM32 a) * (getM41 a)) -
							 ((getM11 a) * (getM32 a) * (getM44 a)) - ((getM12 a) * (getM34 a) * (getM41 a)) - ((getM14 a) * (getM31 a) * (getM42 a))
						m33	=	((getM11 a) * (getM22 a) * (getM44 a)) + ((getM12 a) * (getM24 a) * (getM41 a)) + ((getM14 a) * (getM21 a) * (getM42 a)) -
							 ((getM11 a) * (getM24 a) * (getM42 a)) - ((getM12 a) * (getM21 a) * (getM44 a)) - ((getM14 a) * (getM22 a) * (getM41 a))
						m34	=	((getM11 a) * (getM24 a) * (getM32 a)) + ((getM12 a) * (getM21 a) * (getM34 a)) + ((getM14 a) * (getM22 a) * (getM31 a)) -
							 ((getM11 a) * (getM22 a) * (getM34 a)) - ((getM12 a) * (getM24 a) * (getM31 a)) - ((getM14 a) * (getM21 a) * (getM32 a))

						m41	=	((getM21 a) * (getM33 a) * (getM42 a)) + ((getM22 a) * (getM31 a) * (getM43 a)) + ((getM23 a) * (getM32 a) * (getM41 a)) -
							((getM21 a) * (getM32 a) * (getM43 a)) - ((getM22 a) * (getM33 a) * (getM41 a)) - ((getM23 a) * (getM31 a) * (getM42 a))
						m42	=	((getM11 a) * (getM32 a) * (getM43 a)) + ((getM12 a) * (getM33 a) * (getM41 a)) + ((getM13 a) * (getM31 a) * (getM42 a)) -
							 ((getM11 a) * (getM33 a) * (getM42 a)) - ((getM12 a) * (getM31 a) * (getM43 a)) - ((getM13 a) * (getM32 a) * (getM41 a))
						m43	=	((getM11 a) * (getM23 a) * (getM42 a)) + ((getM12 a) * (getM21 a) * (getM43 a)) + ((getM13 a) * (getM22 a) * (getM41 a)) -
							 ((getM11 a) * (getM22 a) * (getM43 a)) - ((getM12 a) * (getM23 a) * (getM41 a)) - ((getM13 a) * (getM21 a) * (getM42 a))
						m44	=	((getM11 a) * (getM22 a) * (getM33 a)) + ((getM12 a) * (getM23 a) * (getM31 a)) + ((getM13 a) * (getM21 a) * (getM32 a)) -
							 ((getM11 a) * (getM23 a) * (getM32 a)) - ((getM12 a) * (getM21 a) * (getM33 a)) - ((getM13 a) * (getM22 a) * (getM31 a))

						det 	= 	((getM11 a) * m11) + ((getM12 a) * m21) + ((getM13 a) * m31) + ((getM14 a) * m41);
						nm11 	=	m11 / det
						nm12 	=	m12 / det
						nm13 	=	m13 / det
						nm14 	=	m14 / det
						nm21 	=	m21 / det
						nm22 	=	m22 / det
						nm23 	=	m23 / det
						nm24 	=	m24 / det
						nm31 	=	m31 / det
						nm32 	=	m32 / det
						nm33 	=	m33 / det
						nm34 	=	m34 / det
						nm41 	=	m41 / det
						nm42 	=	m42 / det
						nm43 	=	m43 / det
						nm44 	=	m44 / det

identityMat		::	Matrix
identityMat		=	Matrix 1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1

instance FrameRelation Matrix where
	transformEvent a (Event pos time)		=	Event (transMatVec a pos) time
	getInverse a									=	invMat a
	identity											=	identityMat
	
instance FrameRelationComposite Matrix Matrix Matrix where
	composition a b			=	multMat a b
	
instance SpatialFrameRelation Matrix where
	transformTime a b			=	b
	
instance StaticFrameRelation Matrix where
	transformPosition a b	=	transMatVec a b 