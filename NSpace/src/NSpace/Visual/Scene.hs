-----------------------------------------------------------------------------
--
-- Module      :  NSpace.Visual.Scene
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module NSpace.Visual.Scene ( 
	Scene(..),
	renderScene
) where
	
import Graphics.Rendering.OpenGL	

data Scene			=		Scene {
		
	}
	
renderScene			::		Scene -> IO ()
renderScene(x)		=		do
	clear [ColorBuffer]
	renderPrimitive Quads $ do
		color $ (Color3 (1.0::GLfloat) 0 0)
		vertex $ (Vertex3 (0::GLfloat) 0 0)
		vertex $ (Vertex3 (0::GLfloat) 0.2 0)
		vertex $ (Vertex3 (0.2::GLfloat) 0.2 0)
		vertex $ (Vertex3 (0.2::GLfloat) 0 0)
		color $ (Color3 (0::GLfloat) 1 0)
		vertex $ (Vertex3 (0::GLfloat) 0 0)
		vertex $ (Vertex3 (0::GLfloat) (-0.2) 0)
		vertex $ (Vertex3 (0.2::GLfloat) (-0.2) 0)
		vertex $ (Vertex3 (0.2::GLfloat) 0 0)
		color $ (Color3 (0::GLfloat) 0 1)
		vertex $ (Vertex3 (0::GLfloat) 0 0)
		vertex $ (Vertex3 (0::GLfloat) (-0.2) 0)
		vertex $ (Vertex3 ((-0.2)::GLfloat) (-0.2) 0)
		vertex $ (Vertex3 ((-0.2)::GLfloat) 0 0)
		color $ (Color3 (1::GLfloat) 0 1)
		vertex $ (Vertex3 (0::GLfloat) 0 0)
		vertex $ (Vertex3 (0::GLfloat) 0.2 0)
		vertex $ (Vertex3 ((-0.2::GLfloat)) 0.2 0)
		vertex $ (Vertex3 ((-0.2::GLfloat)) 0 0)
	flush
