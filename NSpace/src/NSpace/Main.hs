-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import NSpace.Visual.Scene
import NSpace.Vector
import NSpace.ReferenceFrame
import NSpace.Shape.Volume
import NSpace.Shape.Mesh
import NSpace.Shape.Shape

main = do
	createWindow "NSpace"
	windowSize $= Size 640 480
	displayCallback $= renderScene Scene
	mainLoop
