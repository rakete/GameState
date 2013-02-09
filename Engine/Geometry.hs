
module Engine.Geometry
( newVBO
, Indices(..)
, FaceGroup(..)
, Mesh(..)
, renderMeshWith
, triangulate
, indicesAsList
) where

import Graphics.Rendering.OpenGL.GL hiding (get,newMatrix)

import Data.Maybe
import Foreign
import qualified Data.IntMap as IM

import Math.Vector
import Engine.Texture

-- thanks quicksilver
newVBO :: (Storable a) => BufferTarget -> [a] -> BufferUsage -> IO BufferObject
newVBO buf xs mode = do
    [bo] <- genObjectNames 1
    bindBuffer buf $= Just bo
    withArray xs $ \p ->
        bufferData buf $= (fromIntegral (length xs * sizeOf (head xs)),p,mode)
    return bo

data Indices a =
    TriangleIndices (a,a,a) |
    QuadIndices (a,a,a,a) |
    PolygonIndices [a]
    deriving Show

data FaceGroup = FaceGroup
    { face_offset :: Int
    , face_numIndices :: Int
    , face_material :: Material GLfloat
    }
    deriving Show

data Mesh = Mesh
    { mesh_vertices :: BufferObject
    , mesh_normals :: Maybe BufferObject
    , mesh_texcoords :: Maybe BufferObject
    , mesh_vertexcolors :: Maybe BufferObject
    , mesh_indices :: BufferObject
    , mesh_opaqueFaces :: [FaceGroup]
    , mesh_translucentFaces :: [FaceGroup]
    , mesh_poses :: [(String,BufferObject)]
    , mesh_numIndices :: Int
    , mesh_color :: Maybe (Color4 GLfloat)
    }
    deriving Show

class Geometry geom where
    renderGeometryWith' :: geom -> IO () -> IO ()
    triangulate' :: geom -> geom

renderMeshWith :: Mesh -> IO () -> IO ()
renderMeshWith mesh fio = do
    clientState VertexArray $= Enabled
    clientState IndexArray $= Enabled

    let n = fromIntegral $ mesh_numIndices mesh

    bindBuffer ArrayBuffer $= (Just $ mesh_vertices mesh)
    arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 nullPtr

    _ <- case mesh_normals mesh of
            (Just nid) -> do
                clientState NormalArray $= Enabled
                bindBuffer ArrayBuffer $= Just nid
                arrayPointer NormalArray $= VertexArrayDescriptor 3 Float 0 nullPtr
            otherwise -> return ()

    _ <- case mesh_texcoords mesh of
            (Just tid) -> do
                clientState TextureCoordArray $= Enabled
                bindBuffer ArrayBuffer $= Just tid
                arrayPointer TextureCoordArray $= VertexArrayDescriptor 3 Float 0 nullPtr
            otherwise -> return ()

    bindBuffer ElementArrayBuffer $= (Just $ mesh_indices mesh)
    arrayPointer IndexArray $= VertexArrayDescriptor 1 UnsignedInt 0 nullPtr

    fio

    clientState IndexArray $= Disabled
    clientState TextureCoordArray $= Disabled
    clientState NormalArray $= Disabled
    clientState VertexArray $= Disabled



--
--

triangulate :: (VertexComponent c, RealFloat c, Enum c, Integral a) => IM.IntMap (Vertex3 c) -> Indices a -> (Bool,[Indices a])
triangulate _ t@(TriangleIndices _) = (True,[t])
triangulate _ (QuadIndices (i1,i2,i3,i4)) = (True,[TriangleIndices (i1,i2,i3),TriangleIndices (i1,i3,i4)])
triangulate vertices (PolygonIndices allindices) =
    if n > 3
     then recur allindices Nothing
     else let t = (\(a:b:c:[]) -> (a,b,c)) allindices
          in (True,[TriangleIndices t])

    where

    n = fromIntegral $ IM.size vertices
    discard_component = -- (Bool,Bool,Bool)
        (\v -> (\(x,y,z) -> let b = vecMinC v in (x==b,y==b,z==b)) $ vec3Tuple v) $
            foldl (\v1 v2 -> vecMap (/n) $ v1 `vecAdd` v2) (Vertex3 0.0 0.0 0.0) $
                scanl1 (\v1 v2 -> vecMap abs $ (v1 `vecSub` v2)) $ IM.elems vertices
    flat_vertices = case discard_component of
                        (True,_,_) -> IM.map ((\(Vertex3 _ y z) -> Vertex2 y z)) vertices
                        (_,True,_) -> IM.map ((\(Vertex3 x _ z) -> Vertex2 x z)) vertices
                        (_,_,True) -> IM.map ((\(Vertex3 x y _) -> Vertex2 x y)) vertices

    recur indices@(i1:i2:i3:is) mi =
         let v1 = flat_vertices IM.! (fromIntegral i1)
             v2 = flat_vertices IM.! (fromIntegral i2)
             v3 = flat_vertices IM.! (fromIntegral i3)
             ts = filter (\x -> not (x==i1 || x==i2 || x==i3)) allindices
             vs = map (\i -> flat_vertices IM.! (fromIntegral i)) ts
         in let b' = not $ any (==True) $ map (\v -> insideTriangle2D v1 v2 v3 v) vs
            in if null is
                then (b',[TriangleIndices (i1,i2,i3)])
                else if b'
                      then let (b,tris) = recur (i1:i3:is) (Just i1) -- wrong! this was not what i thought of, but it works...
                           in if b
                               then (b,[TriangleIndices (i1,i2,i3)] ++ tris)
                               else if isJust mi
                                     then if fromJust mi == i1
                                           then (False,(takeachance indices) ++ tris)
                                           else recur ((i2:i3:is) ++ [i1]) mi
                                     else recur ((i2:i3:is) ++ [i1]) (Just i1)
                      else if isJust mi
                            then if fromJust mi == i1
                                  then (False,takeachance indices)
                                  else recur ((i2:i3:is) ++ [i1]) mi
                            else recur ((i2:i3:is) ++ [i1]) (Just i1)

    takeachance indices@(i1:i2:i3:is) =
        if null is
         then [TriangleIndices (i1,i2,i3)]
         else [TriangleIndices (i1,i2,i3)] ++ takeachance (i1:i3:is)

--
--

indicesAsList :: [Indices a] -> [a]
indicesAsList iss = asList iss [] where
    asList ((TriangleIndices (i1,i2,i3)):is) xs = asList is (xs ++ [i1,i2,i3])
    asList ((QuadIndices (i1,i2,i3,i4)):is) xs = asList is (xs ++ [i1,i2,i3,i4])
    asList ((PolygonIndices xs'):is) xs = asList is (xs ++ xs')
    asList [] xs = xs


