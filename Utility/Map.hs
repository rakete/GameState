
module Utility.Map
( listIntMap
, listStringMap
, sequenceMap
, catMaybesMap
, freeKeys
, insertUnique
) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe

import Utility.List
import Utility.Monad

listIntMap :: [a] -> IM.IntMap a
listIntMap xs = IM.fromList $ zip [0..] xs

listStringMap :: (a -> String) -> [a] -> M.Map String a
listStringMap key_f xs =
    let keys = map key_f xs
    in M.fromList $ zip keys xs

sequenceMap :: (Monad m, Ord k) => M.Map k (m a) -> m (M.Map k a)
sequenceMap m =
    let ks = M.keys m
        es = M.elems m
    in do
        es' <- sequence es
        return $ M.fromList $ zip ks es'

catMaybesMap :: (Ord k) => M.Map k (Maybe a) -> M.Map k a
catMaybesMap m = M.fromList $ catMaybes $ map liftTb $ M.assocs m

freeKeys :: (Num k,Enum k,Ord k) => M.Map k a -> [k]
freeKeys m =
    let ks = M.keys m
    in if null ks
        then [0]
        else fillGaps ((last ks)+1) ks

insertUnique :: Ord a => (a,Int) -> b -> M.Map (a,Int) b -> M.Map (a,Int) b
insertUnique k@(a,i) b m =
    case M.lookup k m of
        (Just _) -> insertUnique (a,i+1) b m
        Nothing -> M.insert k b m
