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
                         zoneState        :: M.Map Name Zone,
                         unitStatus       :: M.Map Name Unit, 
                         randomStream     :: [Integer] }
                         

instance Terrain Theater where
    connection t n1 n2 = (lookup n1 (zoneConnectivity t) >>= lookup n2)
    adjacentZones t n1 = case lookup n1 (zoneConnectivity t) of 
                            Just l -> map fst l
                            Nothing -> []
    unitLocation n t   = fromJust $ M.lookup n (unitLocations t) 
    zone name t        = fromJust $ M.lookup name (zoneState t)
    unit n t           = fromJust $ M.lookup n (unitStatus t) 
    allUnitLocations   = M.assocs . unitLocations
    unitsIn t n        = M.keys $ M.filter (== n) (unitLocations t)
    allUnitStatus      = M.assocs . unitStatus

instance BattleMap Theater where
  throwDice     = do t <- S.get
                     let random = randomStream t
                     S.put $ t { randomStream = tail (random) }
                     return $ head (random)
  whereIs uname = S.get >>= (return . unitLocation uname)
  zoneDataFor name    = S.get >>= return . zone name
  setZoneDataFor name z = do t <- S.get 
                             S.put $ t { zoneState = M.adjust (\ _ -> z) name (zoneState t)}
                             return z
  updateMovedUnit u z = do t <- S.get 
                           S.put $ t { unitLocations = M.adjust (\ _ -> zoneName z) (unitName u) (unitLocations t), 
                                       unitStatus    = M.adjust (\ _ -> u) (unitName u) (unitStatus t)}
                           return u
  updateStatusOf u = do t <- S.get
                        S.put $ t { unitStatus = M.adjust (const u) (unitName u) (unitStatus t) }
                        return u
  eliminate u = do t <- S.get 
                   S.put $ t { unitStatus = M.delete (unitName u) (unitStatus t),
                               unitLocations = M.delete (unitName u) (unitLocations t)
                               }
                   return u