{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Math.Vector
( Vec2(..)
, Vec3(..)
, Vec4(..)
, Vector(..)
, Vector2(..)
, Vector3(..)
, Vector4(..)
, insideRect2D
, insideTriangle2D
, angleBetween
) where

import qualified Graphics.Rendering.OpenGL.GL as GL

data Vec2 c = Vec2 !c !c
data Vec3 c = Vec3 !c !c !c
data Vec4 c = Vec4 !c !c !c !c

class (Real c,Floating c,Enum c) => Vector v c | v -> c where
    newVector :: [c] -> v
    fromVector :: (Real c1,Floating c1,Enum c1,Vector v1 c1) => v -> v1
    vecToList ::  (Real c1,Floating c1,Enum c1) => v -> [c1]
    vecSetC :: Vector v1 c => v -> Int -> c -> v1
    vecGetC :: v -> Int -> c
    vecDim :: v -> Int
    vecMap :: (c -> c) -> v -> v
    vecFold :: (c -> c -> c) -> v -> c
    vecSub :: v -> v -> v --(Real c1,Floating c1,Enum c1,Vector v1 c1) => v -> v1 -> v
    vecAdd :: v -> v -> v --(Real c1,Floating c1,Enum c1,Vector v1 c1) => v -> v1 -> v
    dot :: v -> v -> c
    vecMinC :: v -> c
    vecMaxC :: v -> c
    vecLength :: v -> c
    norm :: v -> v
    vecNegate :: v -> v
    crossProduct :: v -> v -> v

    -- these defaults are still untested, i have just written them out after i discovered i could
    -- give default implementations for typeclass functions
    vecSetC v i c = newVector $ snd $
        foldl (\(k,xs) b ->
                  if k==i
                   then (k+1,xs ++ [c])
                   else (k+1,xs ++ [b])
              ) (0,[]) $ vecToList v
    vecGetC v i = ((vecToList v) ++ [0..]) !! i
    vecDim v = length $ vecToList v
    vecMap f v = newVector $ map f $ vecToList v
    vecFold f v = foldl1 f $ vecToList v
    vecSub v1 v2 = newVector $ map (\(c1,c2) -> c1 - c2) $ zip (vecToList v1) (vecToList v2)
    vecAdd v1 v2 = newVector $ map (\(c1,c2) -> c1 + c2) $ zip (vecToList v1) (vecToList v2)
    dot v1 v2 = sum $ map (\(c1,c2) -> c1 * c2) $ zip (vecToList v1) (vecToList v2)
    vecMinC v = minimum $ vecToList v
    vecMaxC v = maximum $ vecToList v
    vecLength v = sqrt $ dot v v
    norm v = vecMap (/(vecLength v)) v
    vecNegate v = vecMap negate v
    crossProduct v w =
        let (x1,y1,z1) = fromVector v
            (x2,y2,z2) = fromVector w
        in newVector [(y1*z2 - z1*y2),(z1*x2 - x1*z2),(x1*y2 - y1*x2)]

--
--

instance (Real c,Floating c,Enum c) => Vector (Vec2 c) c where
    newVector (x:y:_) = Vec2 x y
    vecToList (Vec2 x y) = map realToFrac [x,y]
    fromVector (Vec2 x y) = newVector $ map realToFrac [x,y,0.0,1.0]

instance (Real c,Floating c,Enum c,GL.VertexComponent c) => Vector (GL.Vertex2 c) c where
    newVector (x:y:_) = GL.Vertex2 x y
    vecToList (GL.Vertex2 x y) = map realToFrac [x,y]
    fromVector (GL.Vertex2 x y) = newVector $ map realToFrac [x,y,0.0,1.0]

instance (Real c,Floating c,Enum c) => Vector (GL.Vector2 c) c where
    newVector (x:y:_) = GL.Vector2 x y
    vecToList (GL.Vector2 x y) = map realToFrac [x,y]
    fromVector (GL.Vector2 x y) = newVector $ map realToFrac [x,y,0.0,1.0]

--
--

instance (Real c,Floating c,Enum c) => Vector (Vec3 c) c where
    newVector (x:y:z:_) = Vec3 x y z
    vecToList (Vec3 x y z) = map realToFrac [x,y,z]
    fromVector (Vec3 x y z) = newVector $ map realToFrac [x,y,z,1.0]

instance (Real c,Floating c,Enum c,GL.VertexComponent c) => Vector (GL.Vertex3 c) c where
    newVector (x:y:z:_) = GL.Vertex3 x y z
    vecToList (GL.Vertex3 x y z) = map realToFrac [x,y,z]
    fromVector (GL.Vertex3 x y z) = newVector $ map realToFrac [x,y,z,1.0]

instance (Real c,Floating c,Enum c) => Vector (GL.Vector3 c) c where
    newVector (x:y:z:_) = GL.Vector3 x y z
    vecToList (GL.Vector3 x y z) = map realToFrac [x,y,z]
    fromVector (GL.Vector3 x y z) = newVector $ map realToFrac [x,y,z,1.0]

--
--

instance (Real c,Floating c,Enum c) => Vector (Vec4 c) c where
    newVector (x:y:z:w:_) = Vec4 x y z w
    vecToList (Vec4 x y z w) = map realToFrac [x,y,z,w]
    fromVector (Vec4 x y z w) = newVector $ map realToFrac [x,y,z,w]

instance (Real c,Floating c,Enum c,GL.VertexComponent c) => Vector (GL.Vertex4 c) c where
    newVector (x:y:z:w:_) = GL.Vertex4 x y z w
    vecToList (GL.Vertex4 x y z w) = map realToFrac [x,y,z,w]
    fromVector (GL.Vertex4 x y z w) = newVector $ map realToFrac [x,y,z,w]

instance (Real c,Floating c,Enum c,GL.ColorComponent c) => Vector (GL.Color4 c) c where
    newVector (x:y:z:w:_) = GL.Color4 x y z w
    vecToList (GL.Color4 x y z w) = map realToFrac [x,y,z,w]
    fromVector (GL.Color4 x y z w) = newVector $ map realToFrac [x,y,z,w]

--
--

instance (Real c,Floating c,Enum c) => Vector (c,c) c where
    newVector (x:y:_) = (x,y)
    vecToList (x,y) = map realToFrac [x,y]
    fromVector (x,y) = newVector $ map realToFrac [x,y,0.0,1.0]

instance (Real c,Floating c,Enum c) => Vector (c,c,c) c where
    newVector (x:y:z:_) = (x,y,z)
    vecToList (x,y,z) = map realToFrac [x,y,z]
    fromVector (x,y,z) = newVector $ map realToFrac [x,y,z,1.0]

instance (Real c,Floating c,Enum c) => Vector (c,c,c,c) c where
    newVector (x:y:z:w:_) = (x,y,z,w)
    vecToList (x,y,z,w) = map realToFrac [x,y,z,w]
    fromVector (x,y,z,w) = newVector $ map realToFrac [x,y,z,w]

--
--

class (Vector v c) => Vector2 v c where
    vec2Tuple :: v -> (c,c)

instance (Real c,Floating c, Enum c) => Vector2 (Vec2 c) c where
    vec2Tuple (Vec2 x y) = (x,y)

instance (Real c,Floating c, Enum c) => Vector2 (c,c) c where
    vec2Tuple = id

instance (Real c,Floating c, Enum c) => Vector2 (GL.Vector2 c) c where
    vec2Tuple (GL.Vector2 x y) = (x,y)

instance (Real c,Floating c, Enum c, GL.VertexComponent c) => Vector2 (GL.Vertex2 c) c where
    vec2Tuple (GL.Vertex2 x y) = (x,y)

--
--

class (Vector v c) => Vector3 v c where
    vec3Tuple :: v -> (c,c,c)

instance (Real c,Floating c, Enum c) => Vector3 (Vec3 c) c where
    vec3Tuple (Vec3 x y z) = (x,y,z)

instance (Real c,Floating c, Enum c) => Vector3 (c,c,c) c where
    vec3Tuple = id

instance (Real c,Floating c, Enum c) => Vector3 (GL.Vector3 c) c where
    vec3Tuple (GL.Vector3 x y z) = (x,y,z)

instance (Real c,Floating c, Enum c, GL.VertexComponent c) => Vector3 (GL.Vertex3 c) c where
    vec3Tuple (GL.Vertex3 x y z) = (x,y,z)

--
--

class (Vector v c) => Vector4 v c where
    vec4Tuple :: v -> (c,c,c,c)

instance (Real c,Floating c, Enum c) => Vector4 (Vec4 c) c where
    vec4Tuple (Vec4 x y z w) = (x,y,z,w)

instance (Real c,Floating c, Enum c) => Vector4 (c,c,c,c) c where
    vec4Tuple = id

instance (Real c,Floating c, Enum c, GL.VertexComponent c) => Vector4 (GL.Vertex4 c) c where
    vec4Tuple (GL.Vertex4 x y z w) = (x,y,z,w)

instance (Real c,Floating c, Enum c, GL.ColorComponent c) => Vector4 (GL.Color4 c) c where
    vec4Tuple (GL.Color4 x y z w) = (x,y,z,w)

--
--

insideRect2D :: (Vector2 v c) => v -> v -> v -> Bool
insideRect2D p q v =
    let t = (xp > xq, yp > yq)
        (xp,yp) = fromVector p :: (Float,Float)
        (xq,yq) = fromVector q
        (x,y) = fromVector v
    in case t of
        (False,False) -> (x >= xp && x <= xq && y >= yp && y <= yq)
        (False,True) -> (x >= xp && x <= xq && y <= yp && y >= yq)
        (True,False) -> (x <= xp && x >= xq && y >= yp && y <= yq)
        (True,True) -> (x <= xp && x >= xq && y <= yp && y >= yq)

insideTriangle2D :: (Vector2 v c) => v -> v -> v -> v -> Bool
insideTriangle2D a b c v = (sameSide a b c v) && (sameSide b c a v) && (sameSide c a b v)

angleBetween :: (Fractional c, Vector v c) => v -> v -> c
angleBetween a b = (acos $ ((a `dot` b) / (vecLength a * vecLength b)) ) / (pi/180)

--
--

-- obsolete stuff {{{

intersect2D :: (RealFloat c,Vector v c) => v -> v -> v -> v -> Maybe v
intersect2D a v b w =
    let p@(xp,yp) = fromVector v
        q@(xq,yq) = fromVector w
        mp = slope2D (fromVector a) p
        mq = slope2D (fromVector b) q
        x = ((-1)*mq*xq + yq + mp*xp - yp)/(mp - mq)
    in if isNaN x
        then Nothing
        else Just $ newVector [x,(mp*(x-xp)+yp)]

sameSide :: (Vector v c) => v -> v -> v -> v -> Bool
sameSide p q r v =
    let cp1 = (q `vecSub` p) `crossProduct` (v `vecSub` p)
        cp2 = (q `vecSub` p) `crossProduct` (r `vecSub` p)
    in (cp1 `dot` cp2) >= 0.0

slope2D :: (Vector v c) => v -> v -> c
slope2D = (\v2a v2b -> (\(x,y) -> y/x) $ fromVector (v2a `vecSub` v2b))

-- }}}


