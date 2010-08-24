-- |Contain game elements definitions: zones, units, charts...
module Pieces where
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

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

data UnitType = Artillery 
              | Armoured
              | Infantry
              | Flak
                deriving (Eq, Show, Ord, Read)
                         
data CombatUnit = CU Name UnitState UnitType HQ
                deriving (Eq, Show, Read)

data HQType = Division 
            | Regiment
                deriving (Eq, Show, Ord, Read)
              
data HQ = HQ Name UnitState (Maybe HQ)
        deriving (Eq, Show, Read)

-- Zones

data Control = Contested
             | Commonwealth
             | Germany
             | Inoccupied
          deriving (Eq, Show, Read)

data ZoneType = Clear
                 | Road
                 | City
                 | Village
                 | Rough
                 | Hills
                 | Wood
                 | Mountain 
                 | Port
                 | Beach
                 | Strategic
                 | Objective
          deriving (Eq, Show, Read)
                   
                   
data Zone = Zone Name Control [ZoneType]
          deriving (Eq, Show, Read)

type HasRoad = Bool

type Theater = Gr Zone HasRoad


