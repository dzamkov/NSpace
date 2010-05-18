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
	translateMat,
	lookAtMat,
	alignMat,
	identityMat,
	invMat
) where

import qualified Graphics.Rendering.OpenGL as GL
import NSpace.Event
import NSpace.Vector
import NSpace.ReferenceFrame

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

-- Takes three vectors that correspond to the axis's and a last
-- vector for translations and creates a new matrix representing
-- the coordinate space formed.
							
remapMat				::	Vector -> Vector -> Vector -> Vector -> Matrix
remapMat a b c d	=	Matrix m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44
						where
							m11	=	getX a
							m21	=	getY a
							m31	=	getZ a
							m41	=	0
							
							m12	=	getX b
							m22	=	getY b
							m32	=	getZ b
							m42	=	0
							
							m13	=	getX c
							m23	=	getY c
							m33	=	getZ c
							m43	=	0
							
							m14	=	getX d
							m24	=	getY d
							m34	=	getZ d
							m44	=	1
							
						
transMatVec			::	Matrix -> Vector -> Vector
transMatVec a b		=	Vector nx ny nz
						where 
							nx		=	((getM11 a) * (getX b)) + ((getM12 a) * (getY b)) + ((getM13 a) * (getZ b)) + getM14 a
							ny		=	((getM21 a) * (getX b)) + ((getM22 a) * (getY b)) + ((getM23 a) * (getZ b)) + getM24 a
							nz		=	((getM31 a) * (getX b)) + ((getM32 a) * (getY b)) + ((getM33 a) * (getZ b)) + getM34 a
							
-- Multiplies two matrices together
							
multMat			::	Matrix -> Matrix -> Matrix
multMat a b		= Matrix m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44
					where
						m11 	=	((getM11 a) * (getM11 b)) + ((getM12 a) * (getM21 b)) + ((getM13 a) * (getM31 b)) + ((getM14 a) * (getM41 b))
						m12	=	((getM11 a) * (getM12 b)) + ((getM12 a) * (getM22 b)) + ((getM13 a) * (getM32 b)) + ((getM14 a) * (getM42 b))
						m13	=	((getM11 a) * (getM13 b)) + ((getM12 a) * (getM23 b)) + ((getM13 a) * (getM33 b)) + ((getM14 a) * (getM43 b))
						m14	=	((getM11 a) * (getM14 b)) + ((getM12 a) * (getM24 b)) + ((getM13 a) * (getM34 b)) + ((getM14 a) * (getM44 b))

						m21	=	((getM21 a) * (getM11 b)) + ((getM22 a) * (getM21 b)) + ((getM23 a) * (getM31 b)) + ((getM24 a) * (getM41 b))
						m22	=	((getM21 a) * (getM12 b)) + ((getM22 a) * (getM22 b)) + ((getM23 a) * (getM32 b)) + ((getM24 a) * (getM42 b))
						m23	=	((getM21 a) * (getM13 b)) + ((getM22 a) * (getM23 b)) + ((getM23 a) * (getM33 b)) + ((getM24 a) * (getM43 b))
						m24	=	((getM21 a) * (getM14 b)) + ((getM22 a) * (getM24 b)) + ((getM23 a) * (getM34 b)) + ((getM24 a) * (getM44 b))

						m31	=	((getM31 a) * (getM11 b)) + ((getM32 a) * (getM21 b)) + ((getM33 a) * (getM31 b)) + ((getM34 a) * (getM41 b))
						m32	=	((getM31 a) * (getM12 b)) + ((getM32 a) * (getM22 b)) + ((getM33 a) * (getM32 b)) + ((getM34 a) * (getM42 b))
						m33	=	((getM31 a) * (getM13 b)) + ((getM32 a) * (getM23 b)) + ((getM33 a) * (getM33 b)) + ((getM34 a) * (getM43 b))
						m34	=	((getM31 a) * (getM14 b)) + ((getM32 a) * (getM24 b)) + ((getM33 a) * (getM34 b)) + ((getM34 a) * (getM44 b))
						
						m41	=	((getM41 a) * (getM11 b)) + ((getM42 a) * (getM21 b)) + ((getM43 a) * (getM31 b)) + ((getM44 a) * (getM41 b))
						m42	=	((getM41 a) * (getM12 b)) + ((getM42 a) * (getM22 b)) + ((getM43 a) * (getM32 b)) + ((getM44 a) * (getM42 b))
						m43	=	((getM41 a) * (getM13 b)) + ((getM42 a) * (getM23 b)) + ((getM43 a) * (getM33 b)) + ((getM44 a) * (getM43 b))
						m44	=	((getM41 a) * (getM14 b)) + ((getM42 a) * (getM24 b)) + ((getM43 a) * (getM34 b)) + ((getM44 a) * (getM44 b))
						
-- Transforms the second matrix by the first
						
transMat			:: Matrix -> Matrix -> Matrix
transMat	a b	=	multMat b a
							
invMat			::	Matrix -> Matrix
invMat a			= Matrix nm11 nm12 nm13 nm14 nm21 nm22 nm23 nm24 nm31 nm32 nm33 nm34 nm41 nm42 nm43 nm44
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

						det 	= 	((getM11 a) * m11) + ((getM12 a) * m21) + ((getM13 a) * m31) + ((getM14 a) * m41)
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

translateMat		::	Vector -> Matrix
translateMat a		=	remapMat xVec yVec zVec a
						
-- Takes an up vector and a foward vector and projects
-- the coordinate space based on it

alignMat			::	Vector -> Vector -> Matrix
alignMat a b	=	remapMat x y z zeroVec
					where
						x	=	normVec b
						y	=	normVec $ crossVec a x
						z	=	normVec $ crossVec x y

-- Creates a lookat matrix from the up vector, view position
-- and view target
						
lookAtMat			::	Vector -> Vector -> Vector -> Matrix
lookAtMat a b c	=	transMat (alignMat a (subVec c b)) (translateMat b)
						
identityMat		::	Matrix
identityMat		=	Matrix 1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1

instance FrameRelation Matrix where
	transformEvent a (Event pos time)		=	Event (transMatVec a pos) time
	getInverse a									=	invMat a
	identity											=	identityMat
	
instance FrameRelationComposite Matrix Matrix Matrix where
	composition a b			=	transMat a b
	
instance SpatialFrameRelation Matrix where
	transformTime a b			=	b
	
instance StaticFrameRelation Matrix where
	transformPosition a b	=	transMatVec a b 