{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
module Engine.Physics
( PhysicsEntity(..)
, ODEPhysicsWorld(..)
, ODEPhysicsEntity(..)
, initODEPhysicsEntity
, SimplePhysicsEntity(..)
, initSimplePhysicsEntity) where

import Control.Monad.State.Lazy
import Control.Monad.Identity

import qualified Data.Map as M

import Math.Vector
import Math.Matrix
import Math.Quaternion

import qualified Physics.ODE.Types as ODE
import qualified Physics.ODE.Body as ODE
import qualified Physics.ODE.World as World
import qualified Physics.ODE.Space as ODE

--
--

--class PhysicsWorld w

class (Real c,Floating c,Enum c,Quaternion v c,Vector v c,Monad m)
      => PhysicsEntity p m v c | p -> m v c where
  setPosition :: v -> StateT p m ()
  moveEntity :: v -> StateT p m ()
  rotateEntity :: v -> StateT p m ()
  
  getTransform :: StateT p m (Mat c)
  getWorldRight :: StateT p m v
  getWorldUp :: StateT p m v
  getWorldForward :: StateT p m v
  getLocalRight :: StateT p m v
  getLocalUp :: StateT p m v
  getLocalForward :: StateT p m v
  getPosition :: StateT p m v
  getQuaternion :: StateT p m v

--
--

data ODEPhysicsWorld =
  ODEPhysicsWorld
  {  ode_world :: ODE.World
  ,  ode_spaces :: M.Map String ODE.Space }

data ODEPhysicsEntity =
  ODEPhysicsEntity
  { ode_body :: ODE.Body
  , ode_geom :: [ODE.Geom]
  , ode_rightVector :: (ODE.ODEreal, ODE.ODEreal, ODE.ODEreal, ODE.ODEreal)
  , ode_upVector :: (ODE.ODEreal, ODE.ODEreal, ODE.ODEreal, ODE.ODEreal)
  , ode_forwardVector :: (ODE.ODEreal, ODE.ODEreal, ODE.ODEreal, ODE.ODEreal) }

initODEPhysicsEntity :: ODEPhysicsWorld -> IO ODEPhysicsEntity
initODEPhysicsEntity w = do
  body <- ODE.create $ ode_world w
  return $ ODEPhysicsEntity
        { ode_body = body
        , ode_geom = []
        , ode_rightVector = newVector [1,0,0,0]
        , ode_upVector = newVector [0,1,0,0]
        , ode_forwardVector = newVector [0,0,1,0] }

instance PhysicsEntity ODEPhysicsEntity IO (ODE.ODEreal, ODE.ODEreal, ODE.ODEreal, ODE.ODEreal) ODE.ODEreal where
  setPosition v = do
    body <- gets ode_body
    let (x,y,z) = fromVector v
    liftIO $ ODE.setBodyPosition body x y z
  moveEntity v = do
    body <- gets ode_body
    q <- liftIO $ ODE.getBodyQuaternion body
    t <- liftIO $ ODE.getBodyPosition body
    let v' = rotateVector q v
        (x,y,z) = vecAdd t $ fromVector v'
    liftIO $ ODE.setBodyPosition body x y z
  rotateEntity q = do
    body <- gets ode_body
    q' <- liftIO $ ODE.getBodyQuaternion body
    liftIO $ ODE.setBodyQuaternion body ((fromQuaternion q) `quatProduct` q')

  getTransform = do
    body <- gets ode_body
    (rmat :: [Float]) <- liftIO $ ODE.getBodyQuaternion body >>= return . quatMat
    (tmat :: [Float]) <- liftIO $ ODE.getBodyPosition body >>= return . translationMat
    -- [1.0,0.0,0.0,-2.0,
    --  0.0,1.0,0.0,0.0,
    --  0.0,0.0,1.0,2.0,
    --  0.0,0.0,0.0,1.0]
    liftIO $ print tmat
    return $ fromMatrix $ tmat `matMul` rmat

  getWorldRight = do
    e <- get
    q <- liftIO $ ODE.getBodyQuaternion (ode_body e)
    return $ norm $ rotateVector q (ode_rightVector e)
  getWorldUp = do
    e <- get
    q <- liftIO $ ODE.getBodyQuaternion (ode_body e)
    return $ norm $ rotateVector q (ode_upVector e)
  getWorldForward = do
    e <- get
    q <- liftIO $ ODE.getBodyQuaternion (ode_body e)
    return $ norm $ rotateVector q (ode_forwardVector e)
  getLocalRight = get >>= return . ode_rightVector
  getLocalUp = get >>= return . ode_upVector
  getLocalForward = get >>= return . ode_forwardVector
  getPosition = gets ode_body >>= liftIO . ODE.getBodyPosition >>= return . fromVector
  getQuaternion = gets ode_body >>= liftIO . ODE.getBodyQuaternion >>= return . fromVector


--
--

type SimplePhysicsWorld = M.Map String SimplePhysicsEntity

data SimplePhysicsEntity = SimplePhysicsEntity {
  entity_position :: (Float, Float, Float, Float) ,
  entity_quaternion :: (Float, Float, Float, Float) ,
  entity_rightVector :: (Float, Float, Float, Float) ,
  entity_upVector :: (Float, Float, Float, Float) ,
  entity_forwardVector :: (Float, Float, Float, Float) }
                           
initSimplePhysicsEntity :: SimplePhysicsEntity
initSimplePhysicsEntity = SimplePhysicsEntity  {
  entity_position = newVector [0,0,0] ,
  entity_quaternion = newVector [1,0,0,0] ,
  entity_rightVector = newVector [1,0,0] ,
  entity_upVector = newVector [0,1,0] ,
  entity_forwardVector = newVector [0,0,1] }

-- instance PhysicsEntity SimplePhysicsEntity Identity (Float, Float, Float, Float) Float where

--     setPosition p v = do
--         return p{ entity_position = fromVector v }
--     moveEntity p v = do
--         let q = entity_quaternion p
--         let t = entity_position p
--         let v' = rotateVector q v
--         return p{ entity_position = vecAdd t v' }
--     rotateEntity p q = do
--         let q' = (fromQuaternion q) `quatProduct` (entity_quaternion p)
--         return p{ entity_quaternion = q' }

--     getTransform e =
--         let rmat :: [Float] = quatMat $ entity_quaternion e
--             tmat :: [Float] = translationMat $ entity_position e
--         in return $ fromMatrix $ rmat `matMul` tmat
--     getWorldRight e =
--         let q = entity_quaternion e
--         in return $ norm $ rotateVector q (entity_rightVector e)
--     getWorldUp e =
--         let q = entity_quaternion e
--         in return $ norm $ rotateVector q (entity_upVector e)
--     getWorldForward e =
--         let q = entity_quaternion e
--         in return $ norm $ rotateVector q (entity_forwardVector e)
--     getLocalRight e = return $ entity_rightVector e
--     getLocalUp e = return $ entity_upVector e
--     getLocalForward e = return $ entity_forwardVector e
--     getPosition = return . entity_position
--     getQuaternion = return . entity_quaternion

