-- |Contain game elements definitions: zones, units, charts...
module Pieces where
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import qualified Data.Map as M

-- Troops

data UnitState = UnitState { offense :: Int,
                             defense :: Int,
                             movement :: Int }
                 deriving (Eq, Show, Read)
                          
data State = Full 
           | Reduced 
           | Disorganized
           deriving (Eq, Show, Read)
                    
type Name   = String

data Side = German 
          | British
            deriving (Eq, Show, Read)
                     
data UnitType = Artillery 
              | Armoured
              | Infantry
              | Flak
              | DivisionHQ
              | RegimentHQ
                deriving (Eq, Show, Ord, Read)
                         
data Unit = Unit { unitName     :: Name, 
                   unitSide     :: Side, 
                   unitState    :: State, 
                   unitStrength :: UnitState, 
                   unitType     :: UnitType, 
                   unitHq       :: (Maybe Unit) }
          deriving (Eq, Show, Read)

-- Zones

data Control = Contested
             | Commonwealth
             | Germany
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

type Roads = [(Name,[(Name,Bool)])]

type Zones = M.Map Name Zone

type Units = M.Map Name Unit

-- |Gives the movement cost for a unit moving from given start zone
-- to given target zone on given theater
movementCost :: Roads -> Unit -> Zone -> Zone -> Maybe Integer
movementCost terrain unit z1@(Zone n1 _ _ _ ) z2@(Zone n2 _ ts _) 
  | not $ adjacent terrain n1 n2                       = Nothing
  | Strategic `elem` ts                                = Just 6
  | isConnectedByRoadTo terrain n1 n2                  = Just 1
  | unitType unit == Armoured && not (Road `elem` ts)  = Nothing
  | otherwise                                          = movementCost' z2

movementCost' (Zone _ _ ts Flat)     = Just 1
movementCost' (Zone _ _ ts Rough)    = Just 1
movementCost' (Zone _ _ ts Hilly)    = Just 2
movementCost' (Zone _ _ ts Mountain) = Just 4

isConnectedByRoadTo :: Roads -> Name -> Name -> Bool
isConnectedByRoadTo t n1 n2 = (lookup n1 t >>= lookup n2) == Just True

adjacent :: Roads -> Name -> Name -> Bool
adjacent t n1 n2 = (lookup n1 t >>= lookup n2) /= Nothing

-- for test purpose

germanHQ  = Unit "hq1" German Full (UnitState 0 3 8)  DivisionHQ Nothing
germanArm = Unit "arm1" German Full (UnitState 0 3 8)  Armoured Nothing
rethymnon = Zone "Rethymnon" Unoccupied [City,Port] Flat
beach     = Zone "Beach" Unoccupied [Clear,Port,Beach] Flat
rough     = Zone "Rough" Unoccupied [Clear, Road] Hilly
countryside = Zone "Country" Unoccupied [Wood] Hilly
roads     = [("Rethymnon",[("Beach", True),("Country", False),("Rough", False)])]