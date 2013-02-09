
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL (get)
import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import System.IO
import System.Environment
import Data.Maybe
import Control.Monad.State
import Control.Monad.Trans
import Data.Char
import qualified Data.Map as M

import Math.Vector
import Engine.Texture

data MainState =
    MainState
        { currentGlyphTexture :: Maybe GlyphTexture
        , quitNow :: IORef Bool
        , isDirty :: IORef Bool
        }

initStateIO = do
    quitnow <- newIORef False
    isdirty <- newIORef False
    mousepos <- newIORef (-1,-1)
    return $ MainState
        { currentGlyphTexture = Nothing
        , quitNow = quitnow
        , isDirty = isdirty
        }


terminateGL :: IO ()
terminateGL = do
  -- finish up
  GLFW.closeWindow
  GLFW.terminate

initGL :: StateT MainState IO ()
initGL = do
    quit <- gets quitNow
    dirty <- gets isDirty
    liftIO $ do
        putStr "Initializing GL context..."
        GLFW.initialize
        -- open window
        GLFW.openWindow (Size 200 200) [GLFW.DisplayAlphaBits 8] GLFW.Window
        GLFW.windowTitle $= "FontTest"
        shadeModel $= Flat

        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        clearColor $= Color4 0.0 0.0 0.0 1.0

        texture Texture2D $= Enabled

        GLFW.windowSizeCallback $= (\size@(GL.Size w h) -> do
            let aspect = (1.0*(fromIntegral w)/(fromIntegral h))
            matrixMode $= Projection
            loadIdentity
            viewport $= (Position 0 0, size)
            --perspective 45 aspect 0 2
            ortho2D (-1.0) 1.0 (-1.0) 1.0
            --ortho2D 0 (realToFrac w) (realToFrac h) 0
            )
        GLFW.windowCloseCallback $= (modifyIORef quit (\_ -> True) >> return True)
        putStrLn "finished"

mainLoop :: StateT MainState IO ()
mainLoop = do
    quit <- gets quitNow
    dirty <- gets isDirty
    mainstate <- get
    liftIO $ do
        putStrLn "Entering viewerLoop..."
        -- disable auto polling in swapBuffers
        GLFW.disableSpecial GLFW.AutoPollEvent
        -- mark screen dirty in refresh callback which is often called
        -- when screen or part of screen comes into visibility.
        GLFW.windowRefreshCallback $= (modifyIORef dirty (\_ -> True))
        -- use key callback to track whether ESC is pressed
        GLFW.keyCallback $= (\_ _ -> do
                esc <- GLFW.getKey GLFW.ESC
                if esc == GLFW.Press
                 then modifyIORef quit (\_ -> True)
                 else return ())
        GLFW.mousePosCallback $= (\(Position x y) -> do
             return ())

    loop

    where

    loop :: StateT MainState IO ()
    loop = do
        dirty <- gets isDirty
        quit <- gets quitNow
        mainstate <- get
        liftIO $ do
            GLFW.waitEvents
            d <- readIORef dirty
            if d
             then (render mainstate >> GLFW.swapBuffers)
             else return ()
            modifyIORef dirty (\_ -> False)
        q <- liftIO $ readIORef quit
        if q then return () else loop

render :: MainState -> IO ()
render mainstate = do
    clear [ColorBuffer,DepthBuffer]

    let mglyphtex = currentGlyphTexture mainstate
    case mglyphtex of
        Just glyphtex -> do
            renderText (Vector3 (-0.5) 0.5 0.0) (Vector3 1.0 0.5 0.0) (Vector3 0.0 1.0 (-0.5)) 0.05 glyphtex "This is a Test.\nThis too.\n!\"§$%&/()=?`{[]}"
            renderText (Vector3 (-1.0) 0.0 0.0) (Vector3 1.0 0.0 0.0) (Vector3 0.0 1.0 0.0) 0.1 glyphtex "Just another Test.\nIt seems to be good\nenough for now."
        Nothing -> return ()


loadFont :: String -> String -> StateT MainState IO ()
loadFont fontpaths fontstring = do
    gt <- liftIO $ createGlyphTexture fontpaths fontstring $ (map chr [32..126]) ++ ['ö','ä','ü']
    modify (\s -> s{ currentGlyphTexture = gt })

runMain :: StateT MainState IO ()
runMain = do
  args <- liftIO $ getArgs
  case length args of
    2 -> do
        let (fontpaths:fontstring:_) = args
        initGL
        loadFont fontpaths fontstring
        mainLoop
    otherwise -> do
        liftIO $ print "usage: fonttest fontpath fontname/fontsize"

main = do
    initState <- initStateIO
    runStateT runMain initState

