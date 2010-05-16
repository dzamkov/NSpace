-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Shape.Surface
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Shape.Surface (
	Surface,
	SurfaceMaterial
) where 

import NSpace.ReferenceFrame

-- Represents a surface, which is a dynamic area in spacetime. A surface has
-- no thickness and has an inside and outside.

class (SurfaceMaterial b, FrameRelation c) => Surface a b c where 

-- Represents an oriented substance that makes up a material. Materials exist
-- for every point on the surface.

class SurfaceMaterial a where 