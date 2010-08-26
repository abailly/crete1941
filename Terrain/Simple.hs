{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}
module Terrain.Simple where
import Common
import Terrain
import Units
import Data.Maybe
import qualified Data.Map as M
import qualified Control.Monad.State as S 

data Theater = Theater { zoneConnectivity :: [(Name,[(Name,Bool)])],
                         unitLocations    :: M.Map Name Name,
                         zoneState        :: M.Map Name Zone }
                         

instance Terrain Theater where
    connection t n1 n2 = (lookup n1 (zoneConnectivity t) >>= lookup n2)
    unitLocation n t = fromJust $ M.lookup n (unitLocations t) 
    zone t name = fromJust $ M.lookup name (zoneState t)

instance BattleMap (S.State Theater) Theater where
  whereIs uname = S.get >>= (return . unitLocation uname)
  updateUnitPosition u z = S.get >>= \t -> S.put $ t { unitLocations = M.adjust (\ _ -> zoneName z) (unitName u) (unitLocations t) }