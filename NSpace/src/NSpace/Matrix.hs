-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Matrix
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Matrix (
	Matrix(..),
	transMatVec,
	invMat
) where

import qualified Graphics.Rendering.OpenGL as GL
import NSpace.Vector

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