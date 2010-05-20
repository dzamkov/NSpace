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

import Graphics.Rendering.OpenGL hiding(Matrix)
import Graphics.UI.GLUT hiding(Matrix)
import NSpace.Shape.Mesh
import NSpace.Vector
import NSpace.Matrix
import NSpace.ReferenceFrame

data Scene mesh fr mat tri ver		=		Scene {
		getMesh				::	mesh,
		getCameraFrame		::	ReferenceFrame fr
	}
	
renderScene			:: (Mesh mesh fr mat tri ver) => Scene mesh fr mat tri ver -> IO ()
renderScene x		=		do
	clear [ColorBuffer]
	
	loadIdentity
	perspective (pi / 5) 1 0.01 100.0
	preservingMatrix $ do
		setMatrix $ invMat $ lookAtMat (Vector 0 0 1) (Vector (-10) 0 3) zeroVec
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

setMatrix			::	Matrix -> IO ()
setMatrix a			=	do
								mat <- (newMatrix ColumnMajor (matElems a) :: IO (GLmatrix GLdouble))
								matrix (Nothing)	$=	mat
								