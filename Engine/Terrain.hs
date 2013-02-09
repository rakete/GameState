{-# OPTIONS_GHC -fglasgow-exts #-}
module Engine.Terrain
( createHeightFunctionFromLegend
, createHeightField
) where

import Graphics.Rendering.OpenGL.GL
import Graphics.Imlib
import qualified Data.Array as A
import qualified Data.Map as M
import Foreign

import Engine.Geometry
import Math.Metric
import Utility.Image

-- HeightField stuff {{{

data HeightLegend a = HeightLegend ImlibColor [a] ImlibImage

type HeightFunction a = (ImlibColor -> a)

data HeightField a = HeightField
    { heightfield_array :: A.Array (Int,Int) a
    , heightfield_width :: Int
    , heightfield_height :: Int }

createHeightFunctionFromLegend :: HeightLegend Float -> IO (HeightFunction Float)
createHeightFunctionFromLegend (HeightLegend seperatorcolor knownheights legend) = do
    contextSetImage legend
    w <- imageGetWidth
    h <- imageGetHeight
    colors <- sequence $ [averageColors $ sequence $ [imageQueryPixel x y | x <- [0..(w-1)]] | y <- [0..(h-1)]]
    let m = M.fromList $ (concat $ approximateHeightLegend colors knownheights [])
                            ++ [(ImlibColor 255 0 0 0,minimum knownheights)]
                            ++ [(ImlibColor 255 255 255 255,maximum knownheights)]
    return $! lerpHeightFunction m

    where

    averageColors iocolors = do
        colors <- iocolors
        let l = fromIntegral $ length colors
        return $ (\(ImlibColor a r g b) -> ImlibColor a (r `div` l) (g `div` l) (b `div` l)) $
            foldl (\(ImlibColor a' r' g' b') (ImlibColor a r g b) -> ImlibColor a (r'+r) (g'+g) (b'+b)) (ImlibColor 255 0 0 0) colors

    approximateHeightLegend [] _ hs = hs
    approximateHeightLegend _ [] hs = hs
    approximateHeightLegend (cc:colors) khs hs =
         if cc == seperatorcolor
          then let scs = drop 1 $ dropWhile (not.(==seperatorcolor)) colors
                   khs' = tail khs
                   kh1 = head khs
                   kh2 = head khs'
                   c1 = head colors
                   c2 = head scs
                   colors' = tail colors
                   hs' = hs ++ [[(c1,kh1),(c2,kh2)]]
               in if null scs || null khs'
                   then hs
                   else approximateHeightLegend colors' khs' hs'
          else let (c1,kh1) = head $ last hs
                   (c2,kh2) = last $ last hs
                   f = (\c -> if c==c1 then kh1 else kh2)
                   h' = lerp f c1 c2 cc
                   hs' = (init hs) ++ [((init $ last hs) ++ [(cc,h')] ++ [last $ last hs])]
               in approximateHeightLegend colors khs hs'

    lerpHeightFunction m ic =
        let (c1:c2:_) = bestMatchingColors (M.keys m) ic
            f = (m M.!)
        in case M.lookup ic m of
            Nothing -> if M.null m
                        then 0.0
                        else lerp f c1 c2 ic
            Just h -> h


createHeightField :: HeightFunction Float -> ImlibImage -> IO (HeightField Float)
createHeightField heightfunction heightimage = do
    contextSetImage heightimage
    w <- imageGetWidth
    h <- imageGetHeight

    heights <- mapImage $! (\x y c -> ((x,y),heightfunction c))
    let heightarray = A.array ((0,0),(w-1,h-1)) heights
    --let vertexlist = map (\((x,y),(_,h)) -> Vertex3 (fromIntegral x) h (fromIntegral y)) heights

    return $! HeightField heightarray (w-1) (h-1)

-- }}}

--
--

data SimpleTerrain = SimpleTerrain

createSimpleTerrain :: HeightField Float -> SimpleTerrain
