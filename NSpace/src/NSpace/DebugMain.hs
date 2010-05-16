-----------------------------------------------------------------------------
--
-- Module      :  DebugMain
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

import NSpace.Shape.Volume
import NSpace.Shape.Mesh
import NSpace.Shape.SimpleMesh
import NSpace.Shape.Surface
import NSpace.ReferenceFrame
import NSpace.Vector

testSimpleMesh		=	createSimpleMesh referenceFrame surfaceMaterial verts inds
						where 
							referenceFrame 	=	absoluteFrame :: ReferenceFrame SimpleFrameRelation
							surfaceMaterial	=	SimpleSurfaceMaterial
							verts					=	[Vector 2.0 1.0 4.0, Vector 3.0 2.0 1.0, Vector (-3.0) 3.0 5.0]
							inds					=	[0, 1, 2]
							
showMesh				::	(Mesh mesh fr mat tri ver) => mesh -> String
showMesh a			=	foldl	(\a l -> a ++ (showTri l)) "" (getMeshTriangles a)

showTri				::	(MeshTriangle tri fr mat ver) => tri -> String
showTri a			=	"{" ++ (foldl (\a l -> a ++ (showVer l)) ""  (getTriangleVertices a)) ++ "}"

showVer				::	(MeshVertex ver fr) => ver -> String
showVer a			=	"<" ++ (show $ getX vector) ++ " " ++ (show $ getY vector) ++ " " ++ (show $ getZ vector) ++ ">"
						where
							vector	=	getVertexPosition a