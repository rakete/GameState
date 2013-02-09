module Engine.SceneGraph where

-- Events:
-- Kollisionen zwischen dynamischen Objekten
-- Kollisionen zwischen dynamischen und statischen Objekten
-- Animationen/Effekte/Geraeusche sollen abgespielt werden
-- Flagge wurde erobert, Ziellinie wurde ueberfahren, Startschuss ist gefallen, etc.


import Graphics.Rendering.OpenGL.GL.BasicTypes
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Foreign
import Control.Monad.State

class Uploader a where
    upload :: Renderable o => a -> o

class Renderable o where
    render :: o -> IO ()

class Animatable o where
    animate :: o -> IO ()

data StaticState o = Static
    { staticState_position :: Vector3 GLfloat
    , staticState_orientation :: GLmatrix GLfloat }

data DynamicState = DynamicState
    { dynamicState_position :: Vector3 GLfloat
    , dynamicState_direction :: Vector3 GLfloat
    , dynamicState_orientation :: GLmatrix GLfloat
    , dynamicState_velocity :: GLfloat }

