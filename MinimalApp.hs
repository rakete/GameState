--module Input where

import Data.IORef
import qualified Data.Map as M
import Control.Monad
import System.Exit (ExitCode(..), exitWith)
import Control.Monad.State
import Random

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.StateVar hiding (get)
import qualified Graphics.Rendering.OpenGL.GL.StateVar as GL (get)
import Graphics.Rendering.OpenGL.GL.Framebuffer
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.BasicTypes
import Graphics.Rendering.OpenGL.GL.Rectangles
import Graphics.Rendering.OpenGL.GLU.Matrix

instance Ord GLFW.MouseButton where
    compare a b =
        case (a,b) of
            (GLFW.ButtonLeft,GLFW.ButtonLeft) -> EQ
            (GLFW.ButtonRight,GLFW.ButtonRight) -> EQ
            (GLFW.ButtonMiddle,GLFW.ButtonMiddle) -> EQ
            (GLFW.ButtonLeft,_) -> LT
            (_,GLFW.ButtonLeft) -> GT
            (GLFW.ButtonRight,_) -> LT
            (_,GLFW.ButtonRight) -> GT
            (GLFW.ButtonMiddle,_) -> LT
            (_,GLFW.ButtonMiddle) -> GT
            (GLFW.ButtonNo i,GLFW.ButtonNo k) -> compare i k

data GameInput = GameInput
    { game_dirty :: Bool
    , game_lasttime :: Double
    , game_dtime :: Double
    , game_randomgen :: StdGen
    , mouse_position :: (Position,Maybe Position)
    , mouse_wheel :: (Int,Maybe Int)
    , mouse_buttons :: M.Map GLFW.MouseButton GLFW.KeyButtonState
    }

initGameInput :: StdGen -> GameInput
initGameInput g = GameInput
    { game_dirty = False
    , game_lasttime = 0.0
    , game_dtime = 0.0
    , game_randomgen = g
    , mouse_position = (Position 0 0,Nothing)
    , mouse_wheel = (0,Nothing)
    , mouse_buttons = M.empty
    }

data GameState = GameState
    { dummy :: Int }
    deriving Show

initGameState = GameState
    { dummy = 0 }

initGL gameinputio = do
    putStr "Initializing GL context..."
    GLFW.initialize
    GLFW.openWindow (Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.windowSizeCallback $= (\size@(Size w h) -> do
        let aspect = (1.0*(fromIntegral h)/(fromIntegral w))
        matrixMode $= Projection
        loadIdentity
        )
    GLFW.windowCloseCallback $= (exitWith ExitSuccess)
    GLFW.windowRefreshCallback $= (modifyIORef gameinputio (\s -> s{ game_dirty = True }))
    GLFW.mouseButtonCallback $= (\k a ->
        modifyIORef gameinputio (\i ->
            i{ game_dirty = True, mouse_buttons = M.insert k a $ mouse_buttons i }))
    GLFW.mouseWheelCallback $= (\w -> do
        --w' <- liftM (fst . mouse_wheel) $ readIORef gameinputio
        modifyIORef gameinputio (\i ->
            i{ mouse_wheel = (w, Just $ fst $ mouse_wheel i) }))
    GLFW.mousePosCallback $= (\pos -> do
        --pos' <- liftM (fst . mouse_position) $ readIORef gameinputio
        modifyIORef gameinputio (\i ->
            i{ mouse_position = (pos, Just $ fst $ mouse_position i) }))
    putStrLn "finished"

processEvents :: GameInput -> StateT GameState IO ()
processEvents inp = return ()

inputLoop gameinputio = do
    putStrLn "Entering viewerLoop..."
    -- disable auto polling in swapBuffers
    GLFW.disableSpecial GLFW.AutoPollEvent
    --reactInit (sense gameinputio) (actuate gameinputio) processEvents
    --unStateT (sequence $ repeat $ loop gameinputio) initGameState
    loop gameinputio initGameState

    where

    loop gameinputio lastgamestate = do
        gameinput <- readIORef gameinputio
        (_,newgamestate) <- runStateT (processEvents gameinput) lastgamestate
        render newgamestate >> GLFW.swapBuffers

        GLFW.sleep 0.001
        GLFW.pollEvents

        t <- GL.get GLFW.time
        lt <- liftM game_lasttime $ readIORef gameinputio
        let dt = t - lt
        modifyIORef gameinputio (\s -> s{ game_lasttime = t })
        modifyIORef gameinputio (\s -> s{ game_dtime = dt })

        print dt
        print newgamestate
        loop gameinputio newgamestate

    render gamestate = do
        clear [ColorBuffer]

main = do
    rgen <- newStdGen
    gameinputio <- newIORef $ initGameInput rgen
    initGL gameinputio
    inputLoop gameinputio
