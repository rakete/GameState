{-# LANGUAGE GADTs, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies #-}

import Data.IORef
import Data.STRef
import qualified Data.Map as M
import Control.Monad
import System.Exit (ExitCode(..), exitWith)
import Control.Monad.State.Lazy
import Control.Monad.Identity
import System.Random
import Data.Maybe
import Foreign

import qualified Graphics.UI.GLFW as GLFW
--import Graphics.Rendering.OpenGL.GL.StateVar (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Math.Vector
import Math.Matrix
import Math.Quaternion
import Parser.ObjParser
import Engine.Geometry
import Engine.Game
import Engine.Physics
import Engine.Primitives
import Engine.Input

import Utility.Monad

--
--

initCamera :: IO Camera
initCamera = do
    --m <- newMatrix ColumnMajor identity
    return $ Camera
        { camera_zoom = 0.0
        , camera_fov = 45
        , camera_target = (Nothing,Nothing) }

data Camera = Camera
    { camera_zoom :: GL.GLfloat
    , camera_fov :: GL.GLdouble
    , camera_target :: (Maybe (GL.Vertex3 GL.GLfloat),Maybe String) }

instance GameEntity GameState Camera Camera ODEPhysicsEntity IO where

    render _ _ _ _ = return ()

    viewFrom i o p = do
        let fov = camera_fov o
        size@(GL.Size w h) <- GL.get GLFW.windowSize
        let aspect = (1.0*(fromIntegral w)/(fromIntegral h))
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.viewport $= (GL.Position 0 0, size)
        GL.perspective fov aspect 1 150

        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity
        
        rmat <- evalStateT getQuaternion p >>= return . map realToFrac . quatMat
        tmat :: [GL.GLfloat] <- evalStateT getPosition p >>= return . map realToFrac . translationMat

        m :: GL.GLmatrix GL.GLfloat <- GL.newMatrix GL.RowMajor $ matToList $ (rmat `matMul` tmat)
        GL.multMatrix m
        GL.matrix Nothing $= m


    load name t = addObject $ Object name t

    act inp i o p = do
      liftIO $ evalStateT (getWorldRight >>= moveEntity . vecMap (*(-0.01))) p
        -- let keymap = keyboard_keys inp
        -- let akey = M.lookup (GLFW.CharKey 'A') keymap
        -- let dkey = M.lookup (GLFW.CharKey 'D') keymap
        -- let wkey = M.lookup (GLFW.CharKey 'W') keymap
        -- let skey = M.lookup (GLFW.CharKey 'S') keymap
        -- let upkey = M.lookup (GLFW.SpecialKey GLFW.UP) keymap
        -- let downkey = M.lookup (GLFW.SpecialKey GLFW.DOWN) keymap
        -- let leftkey = M.lookup (GLFW.SpecialKey GLFW.LEFT) keymap
        -- let rightkey = M.lookup (GLFW.SpecialKey GLFW.RIGHT) keymap
        -- case (akey,dkey,wkey,skey) of
        --     (Just ((_,GLFW.Press):_), Just ((_,GLFW.Press):_), _, _) -> return ()
        --     (_, _, Just ((_,GLFW.Press):_), Just ((_,GLFW.Press):_)) -> return ()
        --     (Just ((_,GLFW.Press):_), _, _, _) ->
        --         getWorldRight p >>= moveEntity p . vecMap (*(-0.01))
        --     (_, Just ((_,GLFW.Press):_), _, _) ->
        --         getWorldRight p >>= moveEntity p . vecMap (*0.01)
        --     (_, _, Just ((_,GLFW.Press):_), _) ->
        --         getWorldForward p >>= moveEntity p . vecMap (*(-0.01))
        --     (_, _, _, Just ((_,GLFW.Press):_)) ->
        --         getWorldForward p >>= moveEntity p . vecMap (*0.01)
        --     otherwise -> return ()
        -- -- case (leftkey,rightkey,upkey,downkey) of
            -- (Just ((_,GLFW.Press):_), Just ((_,GLFW.Press):_), _, _) -> return ()
            -- (_, _, Just ((_,GLFW.Press):_), Just ((_,GLFW.Press):_)) -> return ()
            -- (Just ((_,GLFW.Press):_), _, _, _) ->
            --     get >>= lift . getLocalUp >>= rotateEntity . rotationQuat (-0.1)
            -- (_, Just ((_,GLFW.Press):_), _, _) ->
            --     get >>= lift . getLocalUp >>= rotateEntity . rotationQuat 0.1
            -- (_, _, Just ((_,GLFW.Press):_), _) ->
            --     get >>= lift . getWorldRight >>= rotateEntity . rotationQuat (-0.1)
            -- (_, _, _, Just ((_,GLFW.Press):_)) ->
            --     get >>= lift . getWorldRight >>= rotateEntity . rotationQuat 0.1
            -- otherwise -> return ()

--
--

data Grid = Grid
    { grid_geometry :: Mesh }

instance GameEntity GameState PlanePrimitive Grid ODEPhysicsEntity IO where

    render mode _ (Grid mesh) p = do
        let n = fromIntegral $ mesh_numIndices mesh
        renderMeshWith mesh $ do
            GL.lighting $= GL.Disabled
            GL.cullFace $= Nothing
            GL.polygonMode $= (GL.Line,GL.Line)
            GL.lineSmooth $= GL.Disabled

            GL.currentColor $= GL.Color4 0.4 0.2 0.8 1.0
            GL.lineWidth $= 1.0
            GL.drawElements GL.Quads n GL.UnsignedInt nullPtr

            GL.lineSmooth $= GL.Enabled
            GL.polygonMode $= (GL.Fill,GL.Fill)
            GL.cullFace $= Just GL.Back
            GL.lighting $= GL.Enabled

    viewFrom _ _ _ = return ()

    act _ _ _ _ = return ()

    load name t = (liftIO $ construct t) >>= addObject . Object name . Grid

--
--

data Box = Box
    { box_geometry :: Mesh }

instance GameEntity GameState BoxPrimitive Box ODEPhysicsEntity IO where

    render mode _ (Box mesh) p = do
        let n = fromIntegral $ mesh_numIndices mesh
            mcolor = mesh_color mesh

        renderMeshWith mesh $ do
            GL.currentColor $= fromMaybe (GL.Color4 0.0 1.0 0.0 1.0) mcolor
            GL.drawElements GL.Triangles n GL.UnsignedInt nullPtr

            GL.lighting $= GL.Disabled
            GL.currentColor $= GL.Color4 0.0 1.0 0.0 1.0
            GL.polygonOffsetFill $= GL.Enabled
            GL.lineWidth $= 2.0
            GL.lineSmooth $= GL.Disabled
            GL.polygonMode $= (GL.Line,GL.Line)
            GL.polygonOffset $= ((1.0),(1.0))
            GL.drawElements GL.Triangles n GL.UnsignedInt nullPtr
            GL.polygonMode $= (GL.Fill,GL.Fill)
            GL.lineSmooth $= GL.Enabled
            GL.polygonOffsetFill $= GL.Disabled

            GL.lighting $= GL.Enabled

    viewFrom _ _ _ = return ()

    act _ _ _ _ = return ()

    load name t = (liftIO $ construct t) >>= addObject . Object name . Box

--
--



{-loadShader :: [FilePath] -> [FilePath] -> IO Program
loadShader vertexshaderpaths fragmentshaderpaths = do
    vertexshaderids <- loadShaderFiles vertexshaderpaths
    fragmentshaderids <- loadShaderFiles fragmentshaderpaths

    [programid] <- genObjectNames 1 :: IO [Program]
    attachedShaders programid $= (vertexshaderids,fragmentshaderids)
    linkProgram programid
    b2 <- GL.get $ linkStatus programid
    log2 <- GL.get $ programInfoLog programid
    print b2
    mapM_ putStrLn $ lines log2
    return programid

    where

    loadShaderFiles paths = do
        shaderids <- sequence $ map (\path -> do
            source <- liftM lines $ readFile path
            [shaderid] <- genObjectNames 1
            shaderSource shaderid $= source
            compileShader shaderid
            b <- GL.get $ compileStatus shaderid
            log <- GL.get $ shaderInfoLog shaderid
            print b
            mapM_ putStrLn $ lines log
            return shaderid) paths
        return shaderids

loadObjFile :: FilePath -> IO (Maybe (ObjScene GLfloat GLuint))
loadObjFile path = do
    parseResult <- parseObjFile path
    case parseResult of
        Left err -> do
            print err
            return Nothing
        Right objscene -> do
            putStrLn $ "Success loading : " ++ path
            return $ Just objscene-}

initGame :: IORef GameInput -> StateT GameState IO ()
initGame gameinputio = do
    liftIO $ do
        putStr "Initializing GL context..."
        GLFW.initialize
        GLFW.openWindow (GL.Size 1024 768) [GLFW.DisplayAlphaBits 8, GLFW.DisplayDepthBits 8] GLFW.Window
        GLFW.disableSpecial GLFW.AutoPollEvent

        GL.shadeModel $= GL.Smooth
        GL.blend $= GL.Enabled
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        GL.clearColor $= GL.Color4 0.9 0.9 0.9 1.0
        GL.depthMask $= GL.Enabled
        GL.depthFunc $= Just GL.Lequal
        GL.cullFace $= Just GL.Back

        GL.colorMaterial $= Just (GL.Front, GL.Diffuse)
        GL.normalize $= GL.Enabled
        GL.lighting $= GL.Enabled

        GL.ambient (GL.Light 0) $= GL.Color4 0.2 0.2 0.2 1.0
        GL.diffuse (GL.Light 0) $= GL.Color4 0.3 0.3 0.3 1.0
        GL.specular (GL.Light 0) $= GL.Color4 0.4 0.4 0.4 1.0
        GL.position (GL.Light 0) $= GL.Vertex4 4.0 1.0 0.0 1.0
        GL.spotDirection (GL.Light 0) $= GL.Normal3 (-1.0) 0.0 0.0
        GL.spotExponent (GL.Light 0) $= 1.0
        GL.light (GL.Light 0) $= GL.Enabled

        GLFW.windowSizeCallback $= (\size@(GL.Size w h) -> do
            modifyIORef gameinputio (\s -> s{ game_dirty = True }))
        GLFW.windowCloseCallback $= (exitWith ExitSuccess)
        GLFW.windowRefreshCallback $= (modifyIORef gameinputio (\s -> s{ game_dirty = True }))
        GLFW.mouseButtonCallback $= (\k a -> modifyIORef gameinputio (updateMouseButton k a))
        GLFW.keyCallback $= (\k a -> modifyIORef gameinputio (updateKey k a))
        GLFW.mouseWheelCallback $= (\w -> modifyIORef gameinputio (updateMouseWheel w))
        GLFW.mousePosCallback $= (\pos -> modifyIORef gameinputio (updateMousePosition pos))
        putStrLn "finished"

    liftIO initCamera >>=
           load "camera" >>=
           spawn (setPosition (0.0,10.0,0.0,0.0) >> getPosition >>= liftIO . putStrLn . show) >>=
           makePlayer
    load "grid" (PlanePrimitive 10.0 10.0 1.0) >>=
         spawn (setPosition (0.0, 0.0, 0.0, 0.0))
    load "box" (BoxPrimitive 1.0 1.0 1.0 (GL.Color4 1.0 0.0 0.0 1.0)) >>=
             \oref -> sequence [spawn (setPosition (x,z,0.0,0.0)) oref | x <- [-2.0,0.0,2.0], z <- [-2.0,0.0,2.0]]

    return ()

main = startGame initGame



