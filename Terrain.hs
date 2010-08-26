{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Terrain where
import Common
import Units
import qualified Data.Map as M
import Control.Monad.State

class Terrain t where
  -- | Tell whether or not two zones identified by their name are connected or not
  connection  :: t -> Name -> Name -> Maybe Bool
  -- | Gives the current location of the given named unit within the terrain
  unitLocation :: Name  -> t -> Name
  -- | Gives the zone data associated with given name
  zone  :: t -> Name -> Zone
  -- | Gives the unit data associated with given name
  unit  :: Name -> t -> Unit
  
-- | A BattleMap stores the main information of the battle.
class (Terrain t, MonadState t m) => BattleMap m t where 
  -- | Tell whether or not two zones identified by their name are connected or not
  areConnected  :: Name -> Name -> m (Maybe Bool)
  -- | Gives the current location of the given named unit within the terrain
  whereIs :: Name -> m Name
  -- | Gives the zone data associated with given name
  zoneDataFor  :: Name -> m Zone
  -- | Ensure unit's position is modified and its current MPs are depleted 
  -- accordingly
  updateMovedUnit :: Unit -> Zone -> Int -> m Unit
  
  
isConnectedByRoadTo :: (Terrain t) => t -> Name -> Name -> Bool
isConnectedByRoadTo t n1 n2 = connection t n1 n2 == Just True

adjacent :: (Terrain t) => t -> Name -> Name -> Bool
adjacent t n1 n2 = connection t n1 n2 /= Nothing
     
data Control = Occupied (Either Side ())
             | Unoccupied
          deriving (Eq, Show, Read)

data Landscape = Flat
              | Rough
              | Hilly
              | Mountain 
              deriving (Eq, Show, Read)
                   
data TerrainType = Port
                 | Beach
                 | Strategic
                 | Objective
                 | Wood
                 | Road
                 | City
                 | Town
                 | Village
                 | Clear
                   deriving (Eq, Show, Read)
                   
data Zone = Zone { zoneName              :: Name, 
                   zoneControl           :: Control, 
                   zoneTerrainAttributes :: [TerrainType], 
                   zoneScape             :: Landscape }
          deriving (Eq, Show, Read)

type HasRoad = Bool

type Units = M.Map Name Unit
