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
import NSpace.Visual.Scene
import NSpace.ReferenceFrame
import NSpace.Vector

testSimpleMesh		=	createSimpleMesh referenceFrame surfaceMaterial verts inds
						where 
							referenceFrame 	=	absoluteFrame :: ReferenceFrame SimpleFrameRelation
							surfaceMaterial	=	SimpleSurfaceMaterial
							verts					=	[Vector 0.5 0.5 0.0, Vector 0.5 0.0 0.0, Vector 0.0 0.0 0.0]
							inds					=	[0, 1, 2]
							
main					=	showScene (Scene testSimpleMesh)