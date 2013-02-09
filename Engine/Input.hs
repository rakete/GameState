{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Engine.Input
( GameInput(..)
, initGameInput
, MouseDevice(..)
, KeyboardDevice(..)
) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Map as M
import Data.Maybe
import Data.List

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

{-instance Ord GLFW.Key where
    compare a b =
        case (a,b) of
            (GLFW.CharKey a', GLFW.CharKey b') -> compare a' b'
            (GLFW.CharKey _, GLFW.SpecialKey _) -> LT
            (GLFW.SpecialKey _, GLFW.CharKey _) -> GT
            (GLFW.SpecialKey a', GLFW.SpecialKey b') ->
                let ai = fromMaybe 0 $ findIndex (== a') specialKeys
                    bi = fromMaybe 0 $ findIndex (== b') specialKeys
                in compare ai bi

        where

        specialKeys = (GLFW.UNKNOWN):[(GLFW.ESC)..(GLFW.KP_ENTER)]-}

data GameInput = GameInput
    { game_dirty :: !Bool
    , game_time :: !Double
    , game_dtime :: !Double
    , mouse_position :: ![(Double,GL.Position)]
    , mouse_wheel :: ![(Double,Int)]
    , mouse_buttons :: M.Map GLFW.MouseButton [(Double,GLFW.KeyButtonState)]
    , keyboard_keys :: M.Map GLFW.Key [(Double,GLFW.KeyButtonState)] }
    deriving Show
    
initGameInput :: GameInput
initGameInput = GameInput
    { game_dirty = False
    , game_time = 0.0
    , game_dtime = 0.0
    , mouse_position = []
    , mouse_wheel = []
    , mouse_buttons = M.empty
    , keyboard_keys = M.empty }
    
--
--

class Timer i t where
    currentTime :: i -> t
    elapsedTime :: i -> t

instance Timer GameInput Double where
    currentTime gi = game_time gi
    elapsedTime gi = game_dtime gi

class MouseDevice i k s p w | i -> k s p w where
    updateMousePosition :: p -> i -> i
    updateMouseWheel :: w -> i -> i
    updateMouseButton :: k -> s -> i -> i

class KeyboardDevice i k s | i -> k s where
    updateKey :: k -> s -> i -> i

instance MouseDevice GameInput GLFW.MouseButton GLFW.KeyButtonState GL.Position Int where
    updateMousePosition p gi =
        let t = currentTime gi
        in gi{ mouse_position = (t,p):(take 9 $ mouse_position gi) }
    updateMouseWheel w gi =
        let t = currentTime gi
        in gi{ mouse_wheel = (t,w):(take 9 $ mouse_wheel gi) }
    updateMouseButton mb kbs gi =
        let t = currentTime gi
        in gi{ mouse_buttons = M.insertWith (\xs' xs -> xs' ++ (take 9 xs)) mb [(t,kbs)] $ mouse_buttons gi }

instance KeyboardDevice GameInput GLFW.Key GLFW.KeyButtonState where
    updateKey kb kbs gi =
        let t = currentTime gi
        in gi{ keyboard_keys = M.insertWith (\xs' xs -> xs' ++ (take 9 xs)) kb [(t,kbs)] $ keyboard_keys gi }

--
--

class Controller i where
    throttle :: i -> Float
    brake :: i -> Float
    yaw :: i -> Float
    pitch :: i -> Float
    roll :: i -> Float

--instance Controller GameInput where
