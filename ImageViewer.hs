
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL (get)
import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import System.IO
import System.Environment
import Data.Maybe
import Control.Monad.State
import Foreign
import Control.Monad.Trans
import Data.Char

import Graphics.Imlib hiding (loadImage,loadFont)

import Math.Vector
import Engine.Texture

data TextureBuffer =
    TextureBuffer
        { textureId :: TextureObject }

data ViewerState =
    ViewerState
        { currentPath :: Maybe FilePath
        , currentImage :: Maybe ImlibImage
        , imageDimensions :: Maybe (GLint,GLint)
        , currentTexture :: Maybe TextureBuffer
        , currentGlyphTexture :: Maybe GlyphTexture
        , quitNow :: IORef Bool
        , isDirty :: IORef Bool
        , mousePos :: IORef (GLint,GLint)
        }

initStateIO = do
    quitnow <- newIORef False
    isdirty <- newIORef False
    mousepos <- newIORef (-1,-1)
    return $ ViewerState
        { currentPath = Nothing
        , currentImage = Nothing
        , imageDimensions = Nothing
        , currentTexture = Nothing
        , currentGlyphTexture = Nothing
        , quitNow = quitnow
        , isDirty = isdirty
        , mousePos = mousepos
        }


terminateGL :: IO ()
terminateGL = do
  -- finish up
  GLFW.closeWindow
  GLFW.terminate

initGL :: StateT ViewerState IO ()
initGL = do
    mpath <- gets currentPath
    mimage <- gets currentImage
    quit <- gets quitNow
    dirty <- gets isDirty
    let path = fromMaybe "" $ mpath
    liftIO $ do
        putStr "Initializing GL context..."
        GLFW.initialize
        -- open window
        case mimage of
            Just image -> do
                contextSetImage image
                w <- liftM fromIntegral imageGetWidth
                h <- liftM fromIntegral imageGetHeight
                GLFW.openWindowHint $= (GLFW.NoResize,1)
                GLFW.openWindow (Size w h) [GLFW.DisplayAlphaBits 8] GLFW.Window
                ortho2D 0.0 (fromIntegral w) (fromIntegral h) 0.0
                return ()
            Nothing -> do
                GLFW.openWindow (Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
                ortho2D 0.0 400.0 400.0 0.0
        GLFW.windowTitle $= "ImageViewer : " ++ path
        shadeModel $= Flat

        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        clearColor $= Color4 0.0 0.0 0.0 1.0

        texture Texture2D $= Enabled

        {-GLFW.windowSizeCallback $= (\size@(GL.Size _ _) -> do
            w <- liftM fromIntegral imageGetWidth
            h <- liftM fromIntegral imageGetHeight
            GLFW.windowSize $= Size w h
            let aspect = (1.0*(fromIntegral w)/(fromIntegral h))
            matrixMode $= Projection
            loadIdentity
            viewport $= (Position 0 0, size)
            --perspective 45 aspect 0 2
            ortho2D (-1.0) 1.0 (-1.0) 1.0
            --ortho2D 0 (realToFrac w) (realToFrac h) 0
            )-}
        GLFW.windowCloseCallback $= (modifyIORef quit (\_ -> True) >> return True)
        putStrLn "finished"

viewerLoop :: StateT ViewerState IO ()
viewerLoop = do
    quit <- gets quitNow
    dirty <- gets isDirty
    viewerstate <- get
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

    loop :: StateT ViewerState IO ()
    loop = do
        dirty <- gets isDirty
        quit <- gets quitNow
        viewerstate <- get
        liftIO $ do
            GLFW.waitEvents
            d <- readIORef dirty
            if d
             then (render viewerstate >> GLFW.swapBuffers)
             else return ()
            modifyIORef dirty (\_ -> False)
        q <- liftIO $ readIORef quit
        if q then return () else loop

render :: ViewerState -> IO ()
render viewerstate = do
    clear [ColorBuffer,DepthBuffer]
    loadIdentity
    let (w,h) = fromMaybe (0,0) $ imageDimensions viewerstate
    ortho2D 0.0 (fromIntegral w) (fromIntegral h) 0.0
    let mtexbuf = currentTexture viewerstate
    case mtexbuf of
        Just (TextureBuffer texid) -> textureBinding Texture2D $= Just texid
        Nothing -> currentColor $= Color4 1.0 0.0 0.0 1.0

    renderPrimitive Polygon $ do
        texCoord ((TexCoord2 0.0 0.0 ) :: TexCoord2 GLfloat)
        vertex ((Vertex2 0.0 0.0) :: Vertex2 GLfloat)
        texCoord ((TexCoord2 1.0 0.0 ) :: TexCoord2 GLfloat)
        vertex ((Vertex2 (fromIntegral w) 0.0) :: Vertex2 GLfloat)
        texCoord ((TexCoord2 1.0 1.0 ) :: TexCoord2 GLfloat)
        vertex ((Vertex2 (fromIntegral w) (fromIntegral h)) :: Vertex2 GLfloat)
        texCoord ((TexCoord2 0.0 1.0 ) :: TexCoord2 GLfloat)
        vertex ((Vertex2 0.0 (fromIntegral h)) :: Vertex2 GLfloat)

    case currentGlyphTexture viewerstate of
        Just glyphtex -> renderText (Vector3 10.0 10.0 0.0) (Vector3 1.0 0.0 0.0) (Vector3 0.0 (-1.0) 0.0) 20.0 glyphtex "FOO!"
        Nothing -> return ()

setTexture :: StateT ViewerState IO ()
setTexture = do
    mimage <- gets currentImage
    case mimage of
        Just image -> do
            tb <- liftIO $ do
                texid <- uploadTexture image
                return $ TextureBuffer texid
            modify (\v -> v{ currentTexture = Just tb })
        Nothing -> return ()

loadImage :: FilePath -> StateT ViewerState IO (Maybe ImlibLoadError)
loadImage path = do
    --(image,imliberror) <- liftIO $ loadImageWithErrorReturn path
    image <- liftIO $ loadImageImmediatelyWithoutCache path
    let imliberror = ImlibLoadErrorNone
    (r,mdim) <- liftIO $ case imliberror of
            ImlibLoadErrorNone -> do
                contextSetImage image
                w <- liftM fromIntegral imageGetWidth
                h <- liftM fromIntegral imageGetHeight
                return (Nothing, Just (w,h))
            otherwise -> return $ (Just imliberror, Nothing)

    case r of
        Just _ -> return r
        Nothing -> do
            modify (\v -> v{ currentPath = Just path, currentImage = Just image, imageDimensions = mdim })
            return r

loadFont :: String -> String -> StateT ViewerState IO ()
loadFont fontpaths fontstring = do
    gt <- liftIO $ createGlyphTexture fontpaths fontstring $ (map chr [32..126])
    modify (\s -> s{ currentGlyphTexture = gt })

runImageViewer :: StateT ViewerState IO ()
runImageViewer = do
  args <- liftIO $ getArgs
  case args of
    [] -> do
        liftIO $ print "usage: imageviewer <filename>"
    (path:_) -> do
        merror <- loadImage path
        case merror of
            Just err ->
                liftIO $ putStrLn $ show err
            Nothing -> do
                initGL
                setTexture
                loadFont "/usr/share/fonts/truetype/freefont/" "FreeSans/30" -- CRASH!!
                viewerLoop

main = do
    initState <- initStateIO
    runStateT runImageViewer initState

