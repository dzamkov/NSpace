-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Shape.Surface
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Shape.Surface (
	Surface,
	UniformSurface,
	SurfaceMaterial,
	SimpleSurfaceMaterial(..),
	getSurfaceMaterial
) where 

import NSpace.ReferenceFrame
import NSpace.Shape.Shape

-- Represents a surface, which is a dynamic area in spacetime. A surface has
-- no thickness and has an inside and outside.

class (Shape a fr, SurfaceMaterial mat) => Surface a fr mat | a -> mat where 

-- A surface with only up of only one material.

class (Surface a fr mat) => UniformSurface a fr mat where
	getSurfaceMaterial			::	a -> mat

-- Represents an oriented substance that makes up a material. Materials exist
-- for every point on the surface.

class SurfaceMaterial a where 

-- A very simple surface material type that can only have one kind of material 

data SimpleSurfaceMaterial 	=	SimpleSurfaceMaterial

instance SurfaceMaterial SimpleSurfaceMaterial where 