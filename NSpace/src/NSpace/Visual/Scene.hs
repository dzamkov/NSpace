-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Visual.Scene
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Visual.Scene ( 
	Scene(..),
	renderScene,
	showScene
) where

import Graphics.Rendering.OpenGL	
import Graphics.UI.GLUT
import NSpace.Shape.Mesh
import NSpace.Vector

data Scene mesh fr mat tri ver		=		Scene {
		getMesh		::	mesh
	}
	
renderScene			:: (Mesh mesh fr mat tri ver) => Scene mesh fr mat tri ver -> IO ()
renderScene x		=		do
	clear [ColorBuffer]
	renderMesh (getMesh x)
	flush

showScene 			::	(Mesh mesh fr mat tri ver) => Scene mesh fr mat tri ver -> IO ()
showScene x			=	do
								createWindow ""
								windowSize $= Size 640 480
								displayCallback $= (renderScene x)
								mainLoop
	
renderMesh			::	(Mesh mesh fr mat tri ver) => mesh -> IO ()
renderMesh x		=	do renderPrimitive Triangles $ (mapM_ (\x -> renderTri x) (getMeshTriangles x))

renderTri			::	(MeshTriangle tri fr mat ver) => tri -> IO ()
renderTri x			=	do mapM_ (\x -> renderVer x) (getTriangleVertices x)

renderVer			::	(MeshVertex ver fr) => ver -> IO ()
renderVer x			=	do vertex $ toVertex3 (getVertexPosition x)