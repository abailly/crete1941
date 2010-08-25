{-# LANGUAGE FlexibleInstances #-}
module Terrain.Simple where
import Common
import Terrain
import Units
import Data.Maybe
import qualified Data.Map as M

data Theater = Theater { zoneConnectivity :: [(Name,[(Name,Bool)])],
                         unitLocations    :: M.Map Name Name,
                         zoneState        :: M.Map Name Zone }
                         

instance Terrain Theater where
    connection t n1 n2 = (lookup n1 (zoneConnectivity t) >>= lookup n2)
    unitLocation t n = fromJust $ M.lookup n (unitLocations t) 
    zone t name = fromJust $ M.lookup name (zoneState t)
    updateUnitPosition t uname zname = t { unitLocations = M.adjust (\ _ -> zname) uname (unitLocations t) }