-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Shape.SimpleMesh
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Shape.SimpleMesh (
	SimpleMesh,
	SimpleMeshTriangle,
	SimpleMeshVertex,
	createSimpleMesh
) where 

import NSpace.Shape.Shape
import NSpace.Shape.Surface
import NSpace.Shape.Mesh
import NSpace.ReferenceFrame
import NSpace.Vector

-- A simple mesh, created from a set of indices and vertices. The mesh has a uniform surface material.
-- Vertices are specified as vectors relative to a given frame of reference and indicies are specified
-- with a list where consecutive groups of three ints specify which vectors make up a mesh.
	
data SimpleMesh fr mat	=	SimpleMesh {
										getMReferenceFrame		:: ReferenceFrame fr,
										getMMaterial				::	mat,
										getMVertices				::	[Vector],
										getMIndices					::	[Int] }

data SimpleMeshTriangle fr mat	=	SimpleMeshTriangle {
													getTMaterial				::	mat,
													getTReferenceFrame		::	ReferenceFrame fr,
													getTVertices				::	[Vector] }

data SimpleMeshVertex fr	=	SimpleMeshVertex {
											getVReferenceFrame		::	ReferenceFrame fr,
											getVPosition				::	Vector }
											
createSimpleMesh				::	(FrameRelation fr, SurfaceMaterial mat) => (ReferenceFrame fr) -> mat -> [Vector] -> [Int] -> (SimpleMesh fr mat)
createSimpleMesh a b c d	= SimpleMesh a b c d
											
-- SimpleMesh instances
extractTriangles						::	[Vector] -> [Int] -> [[Vector]]
extractTriangles a (x:y:z:s)		=	[a !! x, a !! y, a !! z]:(extractTriangles a s)
extractTriangles a b					=	[]
											
instance (SurfaceMaterial mat, FrameRelation fr) => Mesh (SimpleMesh fr mat) fr mat (SimpleMeshTriangle fr mat) (SimpleMeshVertex fr) where 
	getMeshTriangles a 	=	map (\t -> SimpleMeshTriangle (getMMaterial a) (getMReferenceFrame a) t) $ extractTriangles (getMVertices a) (getMIndices a)

instance (SurfaceMaterial mat, FrameRelation fr) => Surface (SimpleMesh fr mat) fr mat where

instance (SurfaceMaterial mat, FrameRelation fr) => StaticShape (SimpleMesh fr mat) fr where
	getStaticFrame a		=	Just (getMReferenceFrame a)

instance (SurfaceMaterial mat, FrameRelation fr) => Shape (SimpleMesh fr mat) fr where

-- SimpleMeshTriangle instances

instance (SurfaceMaterial mat, FrameRelation fr) => MeshTriangle (SimpleMeshTriangle fr mat) fr mat (SimpleMeshVertex fr) where
	getTriangleVertices a	= map (\v -> SimpleMeshVertex (getTReferenceFrame a) v) (getTVertices a)
	
instance (SurfaceMaterial mat, FrameRelation fr) => Surface (SimpleMeshTriangle fr mat) fr mat where

instance (SurfaceMaterial mat, FrameRelation fr) => StaticShape (SimpleMeshTriangle fr mat) fr where
	getStaticFrame a		=	Just (getTReferenceFrame a)

instance (SurfaceMaterial mat, FrameRelation fr) => Shape (SimpleMeshTriangle fr mat) fr where


-- SimpleMeshVertex instances

instance (FrameRelation fr) => MeshVertex (SimpleMeshVertex fr) fr where
	getReferenceFrame a	=	getVReferenceFrame a
	getVertexPosition a	=	getVPosition a

instance (FrameRelation fr) => StaticShape (SimpleMeshVertex fr) fr where 
	getStaticFrame a		=	Just (getVReferenceFrame a)

instance (FrameRelation fr) => Shape (SimpleMeshVertex fr) fr where 