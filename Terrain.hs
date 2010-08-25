module Terrain where
import Common
import Units
import qualified Data.Map as M

class Terrain t where
  -- | Tell whether or not two zones identified by their name are connected or not
  connection  :: t -> Name -> Name -> Maybe Bool
  
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

type Zones = M.Map Name Zone

type Units = M.Map Name Unit
