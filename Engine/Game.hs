{-# LANGUAGE GADTs, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies #-}

module Engine.Game
( StateTRef(..)
, emptyRef
, Game(..)
, GameState(..)
, GameEntity(..)
, RenderingMode(..)
, Object(..)
, Entity(..)
) where

import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.IORef
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.Maybe
import System.Exit (ExitCode(..), exitWith)
import qualified Physics.ODE.World as ODE
--import Control.Parallel.Strategies

import Math.Matrix
import Math.Vector
import Math.Quaternion

import Engine.Geometry
import Engine.Physics
import Engine.Input

import Utility.Monad
import Utility.Map

--
--

data StateTRef s m a = StateTRef
    { getR :: StateT s m (Maybe a)
    , setR :: a -> StateT s m () }

emptyRef :: Monad m => StateTRef s m a
emptyRef = StateTRef
    { getR = return Nothing
    , setR = \_ -> return () }

data Object g p m where
    Object :: (GameEntity g t o p m) =>
        String -> o -> (Object g p m)

type EntityId = (String,Int)
data Entity g p m where
  Entity :: (GameEntity g t o p m) =>
      EntityId -> o -> p -> (Entity g p m)

instance Eq (Entity g p m) where
    (==) (Entity name1 _ _) (Entity name2 _ _) = name1 == name2

data RenderingMode = Opaque | Translucent | Player

--
--

class Monad m => Game g p m | g -> p m where
    addObject :: (Object g p m) -> StateT g IO (StateTRef g IO (Object g p m))
    addEntity :: (Entity g p m) -> StateT g m (StateTRef g m (Entity g p m))

    spawn :: StateT p m () -> StateTRef g m (Object g p m) -> StateT g m (StateTRef g m (Entity g p m))
    makePlayer :: StateTRef g m (Entity g p m) -> StateT g m ()
    getPlayer :: StateT g m (Entity g p m)

    runGameStep :: GameInput -> StateT g m ()
    renderFrame :: StateT g IO ()
    emptyGame :: IO g
    startGame :: (IORef GameInput -> StateT g IO ()) -> IO ()

data GameState = GameState
    { game_objects :: M.Map String (Object GameState ODEPhysicsEntity IO)
    , game_entities :: M.Map (String,Int) (Entity GameState ODEPhysicsEntity IO)
    , game_player :: StateTRef GameState IO (Entity GameState ODEPhysicsEntity IO)
    , game_world :: ODEPhysicsWorld }


instance Game GameState ODEPhysicsEntity IO where

    addObject o@(Object name _) = do
        modify (\gs -> gs{ game_objects = M.insert name o (game_objects gs) })
        return $ StateTRef
            { getR = gets game_objects >>= return . M.lookup name
            , setR = \o -> modify (\gs -> gs{ game_objects = M.insert name o (game_objects gs)}) }

    addEntity e@(Entity name _ _) = do
        modify (\gs -> gs{ game_entities = insertUnique name e (game_entities gs) })
        return $ StateTRef
            { getR = gets game_entities >>= return . M.lookup name
            , setR = \e -> modify (\gs -> gs{ game_entities = M.insert name e (game_entities gs)}) }

    spawn stp oref = do
        odeworld <- gets game_world
        p <- liftIO $ initODEPhysicsEntity odeworld >>= execStateT stp
        mo <- getR oref
        case mo of
            (Just (Object name o)) -> addEntity (Entity (name,0) o p)
            Nothing -> return $ emptyRef

    makePlayer player = modify (\gs -> gs{ game_player = player })

    getPlayer = do
        mplayer <- gets game_player >>= getR
        case mplayer of
            Just player -> return player
            Nothing -> error "No valid Player!"

    runGameStep inp = do
        return ()

    renderFrame = do
        player@(Entity player_id player_object player_physics) <- getPlayer
        entities <- gets game_entities >>= (return . snd . unzip . M.toList)
        liftIO $ do
            GL.clear [GL.ColorBuffer,GL.DepthBuffer]
            viewFrom player_id player_object player_physics

            mapM_ (\e@(Entity i o p) -> GL.preservingMatrix $ do
                m :: GL.GLmatrix GL.GLfloat <- evalStateT getTransform p >>= GL.newMatrix GL.RowMajor . map realToFrac . matToList
                GL.multMatrix m

                case (e == player) of
                    False -> render Opaque i o p
                    True -> render Player i o p) entities

            GLFW.swapBuffers

    emptyGame = do
        w <- ODE.create
        --ODE.setGravity w 0.0 0.0 0.0
        let odeworld = ODEPhysicsWorld
                           { ode_world = w
                           , ode_spaces = M.empty }
        return $ GameState
            { game_objects = M.empty
            , game_entities = M.empty
            , game_player = emptyRef
            , game_world = odeworld }

    startGame init = do
        gameinputio <- newIORef initGameInput

        putStrLn "Starting..."
        emptyGame >>= execStateT (init gameinputio) >>= loop gameinputio

        where

        loop :: IORef GameInput -> GameState -> IO ()
        loop gameinputio lastgamestate = do

            newgamestate <- readIORef gameinputio >>= \inp -> execStateT (runGameStep inp) lastgamestate
            evalStateT renderFrame newgamestate

            GLFW.sleep 0.001
            GLFW.pollEvents

            t <- GL.get GLFW.time
            t `seq` modifyIORef gameinputio (\inp -> inp `seq` inp{ game_time = t, game_dtime = t - (game_time inp) })

            loop gameinputio newgamestate
--
--


class (Monad m, Game g p m) => GameEntity g t o p m | t -> g o p m, o -> g t p m, g -> m where
    render :: RenderingMode -> EntityId -> o -> p -> IO ()
    viewFrom :: EntityId -> o -> p -> IO ()
    act :: GameInput -> EntityId -> o -> p -> StateT g m ()
    load :: String -> t -> StateT g m (StateTRef g m (Object g p m))

