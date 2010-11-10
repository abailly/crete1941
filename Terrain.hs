{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Terrain where
import Common
import Units
import qualified Data.Map as M
import qualified Control.Monad.State as C
import qualified Data.Generics as G

class Terrain t where
  -- | Tell whether or not two zones identified by their name are connected
  -- and if true whether or not this connection is done by road.
  -- @return Nothing if the two zones are not connected, Just False if they are connected,
  -- Just True if they are connected by road.
  connection  :: t -> Name -> Name -> Maybe Bool
  -- | Gives the current location of the given named unit within the terrain
  unitLocation :: Name  -> t -> Name
  -- | Gives the zone data associated with given name
  zone  :: t -> Name -> Zone
  -- | Gives the names of unit located in some zone
  unitsIn  :: t -> Name -> [Name]
  -- | Gives the unit data associated with given name
  unit  :: Name -> t -> Unit
  -- | Give the location of all the units
  allUnitLocations :: t -> [(Name,Name)]
  -- | Give the status of all the units
  allUnitStatus :: t -> [(Name,Unit)]
  
-- | A BattleMap stores the main information of the battle.
class (Terrain t) => BattleMap t where 
  -- | Tell whether or not two zones identified by their name are connected or not
  areConnected  :: Name -> Name -> Battle t (Maybe Bool)
  -- | Gives the current location of the given named unit within the terrain
  whereIs :: Name -> Battle t Name
  -- | Gives the current status of the given named unit within the terrain
  statusOf :: Name -> Battle t Unit
  -- | Unit is eliminated
  eliminate :: Unit -> Battle t Unit
  -- | Change the status of the unit 
  updateStatusOf :: Unit -> Battle t Unit
  -- | Gives the zone data associated with given name
  zoneDataFor  :: Name -> Battle t Zone
  -- | Ensure unit's position is modified.
  updateMovedUnit :: Unit -> Zone -> Battle t Unit
  -- | Get a single dice throw 
  throwDice :: Battle t Integer
  
-- |Concrete type holding the state of the ongoing battle
-- Implements state monad.
newtype (BattleMap t) => Battle t a = Battle { runBattle :: C.State t a}
                     deriving (Monad, C.MonadState t)
                              
isConnectedByRoadTo :: (Terrain t) => t -> Name -> Name -> Bool
isConnectedByRoadTo t n1 n2 = connection t n1 n2 == Just True

adjacent :: (Terrain t) => t -> Name -> Name -> Bool
adjacent t n1 n2 = connection t n1 n2 /= Nothing
     
data Control = Occupied (Either Side ())
             | Unoccupied
          deriving (Eq, Show, Read, G.Data, G.Typeable)

data Landscape = Flat
              | Rough
              | Hilly
              | Mountain 
              deriving (Eq, Show, Read, G.Data, G.Typeable)
                   
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
                   deriving (Eq, Show, Read, G.Data, G.Typeable)
                   
data Zone = Zone { zoneName              :: Name, 
                   zoneControl           :: Control, 
                   zoneTerrainAttributes :: [TerrainType], 
                   zoneScape             :: Landscape }
          deriving (Eq, Show, Read, G.Data, G.Typeable)

type HasRoad = Bool

type Units = M.Map Name Unit
