-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  AllRightsReserved
--
-----------------------------------------------------------------------------

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main = do
    createWindow "Hello World"
    displayCallback $= clear [ ColorBuffer ]
    mainLoop
