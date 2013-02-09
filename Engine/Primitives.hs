
module Engine.Primitives 
( MeshFactory(..)
--, GridPrimitive(..)
, PlanePrimitive(..)
, BoxPrimitive(..)
--, SpherePrimitive(..)
) where

import qualified Graphics.Rendering.OpenGL.GL as GL

import qualified Data.Map as M
import Foreign.Storable

import Engine.Factory
import Engine.Geometry
import Parser.ObjParser
import Math.Vector
import Utility.List

class MeshFactory t where
    construct :: t -> IO Mesh

{-data GridPrimitive = GridPrimitive (Bool, Bool, Bool) GL.GLfloat GL.GLfloat GL.GLfloat

instance MeshFactory GridPrimitive where
    construct (GridPrimitive axis a_size b_size spacing) = do
        let (vs,is) = (create2DGrid axis a_size b_size spacing (GL.Vertex3 0 0 0)) :: ([GL.Vertex3 GL.GLfloat],[GL.GLuint])
        vid <- newVBO GL.ArrayBuffer vs GL.StaticDraw
        iid <- newVBO GL.ElementArrayBuffer is GL.StaticDraw
        return $ Mesh vid Nothing Nothing Nothing iid [] [] [] (length is) Nothing

create2DGrid :: (Enum a, Vector v1 a, Vector v2 a, Integral i,VertexComponent a) =>
    (Bool, Bool, Bool) -> a -> a -> a -> v1 -> ([v2], [i])
create2DGrid axis a_size b_size spacing pos = create where
    GL.Vertex3 x_pos y_pos z_pos = fromVector $ pos

    cs c_pos c_size = 
        [(c_pos - (c_size/2)),((c_pos - (c_size/2))+spacing)..(c_pos + (c_size/2))]

    createIndices as bs (i:is) (x:xs) | i == (length as) * (length bs) = []
                                      | mod i (length as) == 0 || mod i (length as) == (length as)-1 = 
                                            [x] ++ createIndices as bs is xs
                                      | otherwise = [x,x] ++ (createIndices as bs is xs)

    indicesB as bs (i:n:is) = 
        (take (length bs) $ iterate (+(length as)) i) ++ (indicesB as bs (n:is))

    createIndex as bs = 
        (createIndices as bs [0..] [0..]) ++ (createIndices as bs [0..] (indicesB as bs [0..]))

    create = case axis of
            (False,_,_) -> 
                let as = cs y_pos a_size
                    bs = cs z_pos b_size
                in ([newVector (x:y:z:[0.0]) | x <- [x_pos], y <- as, z <- bs],
                    map fromIntegral $ createIndex as bs)
            (_,False,_) -> 
                let as = cs x_pos a_size
                    bs = cs z_pos b_size
                in ([newVector (x:y:z:[0.0]) | x <- as, y <- [y_pos], z <- bs],
                    map fromIntegral $ createIndex as bs)
            (_,_,False) -> 
                let as = cs x_pos a_size
                    bs = cs y_pos b_size
                in ([newVector (x:y:z:[0.0]) | x <- as, y <- bs, z <- [z_pos]],
                    map fromIntegral $ createIndex as bs)
            otherwise -> ([],[])-}


--
--

data PlanePrimitive = PlanePrimitive GL.GLfloat GL.GLfloat GL.GLfloat

instance MeshFactory PlanePrimitive where
    construct (PlanePrimitive asize bsize spacing) = do
        let (a1,a2) = (0 - asize/2, 0 + asize/2)
            (b1,b2) = (0 - bsize/2, 0 + bsize/2)
            xs = [a1,a1+spacing..a2]
            zs = [b1,b1+spacing..b2]
            vs = [ GL.Vertex3 x 0.0 z | x <- xs, z <- zs ]
            ns = [ GL.Vertex3 x 1.0 z | (GL.Vertex3 x _ z) <- vs ]
            u = fromIntegral $ length xs
            v = fromIntegral $ length zs
            is = genIndices u v 0 1 1

        vid <- newVBO GL.ArrayBuffer vs GL.StaticDraw
        nid <- newVBO GL.ArrayBuffer ns GL.StaticDraw
        iid <- newVBO GL.ElementArrayBuffer is GL.StaticDraw

        return $ Mesh vid (Just nid) Nothing Nothing iid [] [] [] (length is) Nothing

        where

        genIndices :: GL.GLuint -> GL.GLuint -> GL.GLuint -> GL.GLuint -> GL.GLuint -> [GL.GLuint]
        genIndices u v i x y | x == u = []
                             | y == v = genIndices u v (i+1) (x+1) 1
                             | otherwise = [i, i+v, i+v+1, i+1] ++ genIndices u v (i+1) x (y+1)


--
--

data BoxPrimitive = BoxPrimitive GL.GLfloat GL.GLfloat GL.GLfloat (GL.Color4 GL.GLfloat)

instance MeshFactory BoxPrimitive where
    construct (BoxPrimitive h w d color) = do
        let (h1,h2) = (0 - h/2, 0 + h/2)
            (w1,w2) = (0 - w/2, 0 + w/2)
            (d1,d2) = (0 - d/2, 0 + d/2)
            vs =
                [ GL.Vertex3 x y z | x <- [w1], y <- [h1,h2], z <- [d1,d2] ] ++
                [ GL.Vertex3 x y z | x <- [w2], y <- [h1,h2], z <- [d1,d2] ] ++
                [ GL.Vertex3 x y z | x <- [w1,w2], y <- [h1], z <- [d1,d2] ] ++
                [ GL.Vertex3 x y z | x <- [w1,w2], y <- [h2], z <- [d1,d2] ] ++
                [ GL.Vertex3 x y z | x <- [w1,w2], y <- [h1,h2], z <- [d1] ] ++
                [ GL.Vertex3 x y z | x <- [w1,w2], y <- [h1,h2], z <- [d2] ]
            is = ([0,1,2
                  ,3,2,1
                  ,4,6,5
                  ,7,5,6
                  ,8,10,9
                  ,11,9,10
                  ,12,13,14
                  ,15,14,13
                  ,20,22,21
                  ,23,21,22
                  ,18,16,19
                  ,17,19,16] :: [GL.GLuint])
            ns =
                [ GL.Vertex3 x y z | x <- [w1 - 1.0], y <- [h1,h2], z <- [d1,d2] ] ++
                [ GL.Vertex3 x y z | x <- [w2 + 1.0], y <- [h1,h2], z <- [d1,d2] ] ++
                [ GL.Vertex3 x y z | x <- [w1,w2], y <- [h1 - 1.0], z <- [d1,d2] ] ++
                [ GL.Vertex3 x y z | x <- [w1,w2], y <- [h2 + 1.0], z <- [d1,d2] ] ++
                [ GL.Vertex3 x y z | x <- [w1,w2], y <- [h1,h2], z <- [d1 - 1.0] ] ++
                [ GL.Vertex3 x y z | x <- [w1,w2], y <- [h1,h2], z <- [d2 + 1.0] ]

        vid <- newVBO GL.ArrayBuffer vs GL.StaticDraw
        nid <- newVBO GL.ArrayBuffer ns GL.StaticDraw
        iid <- newVBO GL.ElementArrayBuffer is GL.StaticDraw

        return $ Mesh vid (Just nid) Nothing Nothing iid [] [] [] (length is) (Just color)

--
--

data SpherePrimitive = SpherePrimitive GL.GLfloat GL.GLuint GL.GLuint (GL.Color4 GL.GLfloat)

instance MeshFactory SpherePrimitive where
    construct (SpherePrimitive r u v color) = do
        let dtheta = fromIntegral $ 180 `div` v
            dphi = fromIntegral $ 360 `div` u
            vs = (do
                    theta <- [((-90.0)+dtheta)..(90.0-dtheta)]
                    phi <- [0..(360-dphi)]
                    return $ GL.Vertex3 ((cos theta)*(cos phi)*(sqrt r)) ((cos theta)*(sin phi)*(sqrt r)) ((sin theta)*(sqrt r))
                 )
                 --[ GL.Vertex3 ((cos (-90.0))*(sqrt r)) ((cos (-90.0))*(sqrt r)) ((sin (-90.0))*(sqrt r)) 
                 --, GL.Vertex3 ((cos 90.0)*(sqrt r)) ((cos 90.0)*(sqrt r)) ((sin 90.0)*(sqrt r)) ]
            is = genIndices 1 1 1

        vid <- newVBO GL.ArrayBuffer vs GL.StaticDraw
        iid <- newVBO GL.ElementArrayBuffer is GL.StaticDraw

        return $ Mesh vid Nothing Nothing Nothing iid [] [] [] (length is) (Just color)

        where

        genIndices :: GL.GLuint -> GL.GLuint -> GL.GLuint -> [GL.GLuint]
        genIndices i x y | x == u = []
                         | y == v = genIndices (i+1) (x+1) 1
                         | x == u-1 = [i, i+1, y+1, y] ++ genIndices (i+1) x (y+1)
                         | otherwise = [i, i+1, i+1+v, i+v] ++ genIndices (i+1) x (y+1)




