import qualified Graphics.Rendering.OpenGL as GL
import Convertions

import Graphics.UI.GLUT
import Graphics.Rendering.FTGL
import Control.Monad
import Data.IORef
import Display
import Bindings
import Cube
import Points

main :: IO ()
main = do
    let a = (integerToBinary (2^73)) `binarySum` (integerToBinary (2^73))
        b = (integerToBinary (2^74)) `binaryRest` (integerToBinary (2^73))
    windowP $ show $ binaryToInteger a

-- Window Section --
windowP :: String -> IO ()
windowP t1 = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "Thony is Love, Thony is Life"
    reshapeCallback $= Just reshape
    depthFunc $= Just Less
    angle <- newIORef 0.0
    delta <- newIORef 0.1
    pos <- newIORef (0, 0)
    font <- createExtrudeFont "PressStart2P.ttf"
    setFontFaceSize font 1 1
    setFontDepth font 1.0
    keyboardMouseCallback $= Just (keyboardMouse delta pos)
    idleCallback $= Just (idle angle delta)
    displayCallback $= display angle pos (font, t1)
    mainLoop
