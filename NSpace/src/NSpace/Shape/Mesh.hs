-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Shape.Mesh
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Shape.Mesh (
	Mesh,
	MeshTriangle,
	MeshVertex,
	getMeshTriangles,
	getTriangleVertices,
	getVertexPosition,
	getReferenceFrame
) where 

import NSpace.Shape.Shape
import NSpace.Shape.Surface
import NSpace.ReferenceFrame
import NSpace.Vector

-- Represents a mesh, which is a static surface made of triangles which
-- can have a material on them.

class (StaticShape a fr, Surface a fr mat, MeshTriangle tri fr mat ver) => Mesh a fr mat tri ver | a -> tri where
	getMeshTriangles			::	a -> [tri]

-- A triangle within a mesh, defined by three verticies. The order of
-- the verticies determines which side of the triangle points out and which points
-- in.
	
class (StaticShape a fr, Surface a fr mat, MeshVertex ver fr) => MeshTriangle a fr mat ver | a -> ver where 
	getTriangleVertices		:: a -> [ver]

-- A vertex in a mesh.

class (StaticShape a fr) => MeshVertex a fr where 
	getVertexPosition			::	a -> Vector
	getReferenceFrame			::	a -> ReferenceFrame fr