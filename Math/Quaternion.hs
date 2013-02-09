{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
module Math.Quaternion
( Quaternion(..)
, rotateQuatGL
, rotateVector
, rotationQuat
, quatProduct
, quatMat
) where

import qualified Graphics.Rendering.OpenGL.GL as GL

import Math.Vector
import Math.Matrix

quatProduct :: (Quaternion q c) => q -> q -> q
quatProduct q1 q2 =
    let (x1,y1,z1,w1) = quatToTuple q1
        (x2,y2,z2,w2) = quatToTuple q2
        qw = w1*w2 - x1*x2 - y1*y2 - z1*z2
        qx = w1*x2 + x1*w2 + y1*z2 - z1*y2
        qy = w1*y2 - x1*z2 + y1*w2 + z1*x2
        qz = w1*z2 + x1*y2 - y1*x2 + z1*w2
    in newQuaternion [qx,qy,qz,qw]

quatDot :: (Quaternion q c) => q -> q -> c
quatDot q1 q2 =
    let (x1,y1,z1,w1) = quatToTuple q1
        (x2,y2,z2,w2) = quatToTuple q2
    in w1*w2 + x1*x2 + y1*y2 + z1*z2

quatInv :: (Quaternion q c) => q -> q
quatInv q = vecMap (*(1/(q `quatDot` (quatConjugate q)))) (quatConjugate q)

quatMat :: (Quaternion q c, Matrix m c) => q -> m
quatMat q = newMatrix $ quatMatList q

-- http://www.genesis3d.com/~kdtop/Quaternions-UsingToRepresentRotation.htm
quatMatList :: (Quaternion q c) => q -> [c]
quatMatList q =
    let (x,y,z,w) = quatToTuple q
    in [ w*w + x*x - y*y - z*z, 2*x*y + 2*w*z        , 2*x*z - 2*w*y        , 0
       , 2*x*y - 2*w*z        , w*w - x*x + y*y - z*z, 2*y*z + 2*w*x        , 0
       , 2*x*z + 2*w*y        , 2*y*z - 2*w*x        , w*w - x*x - y*y + z*z, 0
       , 0                    , 0                    , 0                    , w*w + x*x + y*y + z*z ]

-- http://www.flipcode.com/documents/matrfaq.html#Q54
--quatMatList q =
--    let (x,y,z,w) = quatToTuple q
--   in [ 1 - 2*y^2 - 2*z^2, 2*x*y - 2*w*z    , 2*x*z + 2*w*y    , 0
--      , 2*x*y + 2*w*z    , 1 - 2*x^2 - 2*z^2, 2*y*z - 2*w*x    , 0
--      , 2*x*z - 2*w*y    , 2*y*z + 2*w*x    , 1 - 2*x^2 - 2*y^2, 0
--      , 0                , 0                , 0                , 1 ]

quatConjugate :: (Quaternion q c) => q -> q
quatConjugate q =
    let (x,y,z,w) = quatToTuple q
    in newQuaternion [(-x),(-y),(-z),w]

quatMagnitude :: (Quaternion q c) => q -> c
quatMagnitude q =
    let (x,y,z,w) = quatToTuple q
    in sqrt (w*w + x*x + y*y + z*z)

rotationQuat :: (Quaternion q c, Vector v c) => c -> v -> q
rotationQuat angle' u' =
    let u@(GL.Vector3 ux uy uz) = norm $ fromVector u'
        angle = pi/(360/angle')
    in newQuaternion [(ux * sin(angle)),(uy * sin(angle)),(uz * sin(angle)),(cos(angle))]

rotateVector :: (Quaternion q c, Vector v c1) => q -> v -> v
rotateVector q v = fromVector $ q `quatProduct` (fromVector v) `quatProduct` (quatInv q)

class Vector4 q c => Quaternion q c where
    newQuaternion :: [c] -> q
    fromQuaternion :: (Quaternion q1 c1) => q -> q1
    quatToList :: q -> [c]
    quatToTuple :: q -> (c,c,c,c)

instance (Real c,Enum c,Floating c,GL.VertexComponent c) => Quaternion (GL.Vertex4 c) c where
  newQuaternion xs | not(length xs == 4) = error "newQuaternion: list length must be 4"
                   | otherwise = let (x:y:z:w:_) = xs
                                 in GL.Vertex4 x y z w
  fromQuaternion (GL.Vertex4 x y z w) = newQuaternion $ map realToFrac [x,y,z,w]
  quatToList (GL.Vertex4 x y z w) = [x,y,z,w]
  quatToTuple (GL.Vertex4 x y z w) = (x,y,z,w)

instance (Real c,Enum c,Floating c) => Quaternion (c,c,c,c) c where
  newQuaternion xs | not(length xs == 4) = error "newQuaternion: list length must be 4"
                   | otherwise = let (x:y:z:w:_) = xs
                                 in (x,y,z,w)
  fromQuaternion (x,y,z,w) = newQuaternion $ map realToFrac [x,y,z,w]
  quatToList (x,y,z,w) = [x,y,z,w]
  quatToTuple = id

-- instance (Real c, Floating c, Vector v c) => Quaternion v c where
--    newQuaternion xs | not(length xs == 4) = error "newQuaternion: list length must be 4"
--                     | otherwise = newVector xs
--    fromQuaternion v = newQuaternion $ map realToFrac $ vecToList v
--    quatToList = vecToList
--    quatToTuple v = let (x:y:z:w:_) = vecToList v
--                    in (x,y,z,w)

-- Constraint is no smaller than the instance head
--    in the constraint: Vector v c
--  (Use -XUndecidableInstances to permit this)

rotateQuatGL :: GL.Vertex4 GL.GLfloat -> IO ()
rotateQuatGL q = do
    (m :: GL.GLmatrix GL.GLfloat) <- GL.newMatrix GL.RowMajor $ quatMat q
    GL.multMatrix m
