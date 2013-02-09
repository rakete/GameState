module Engine.OctTree where

import Data.ByteString
import Data.Word

data Geode a =
    GeodeTree
        { one :: OctTree a
        , two :: OctTree a
        , three :: OctTree a
        , four :: OctTree a
        , five :: OctTree a
        , six :: OctTree a
        , seven :: OctTree a
        , eight :: OctTree a
        }
    | Geode
        { x_edges :: (a,a,a,a)
        , y_edges :: (a,a,a,a)
        , z_edges :: (a,a,a,a)
        }
    | FullGeode
    | EmptyGeode

data OctTree a = OctTree
    { level :: Int
    , geode :: Geode a
    }

testOctTree :: OctTree Word8
testOctTree = 
    OctTree 0 $
        GeodeTree EmptyCube EmptyCube EmptyCube EmptyCube EmptyCube EmptyCube EmptyCube EmptyCube


