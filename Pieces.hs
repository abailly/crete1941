-- |Contain game elements definitions: zones, units, charts...
module Pieces where

data UnitState = UnitState { offense :: Int,
                             defense :: Int,
                             movement :: Int }
                 deriving (Show, Read)
                          
data State = Full | Reduced 
           deriving (Show, Read)
                    
class Unit u where
  -- | Provide identification for the Unit
  name :: u -> Name
  -- | Provide unit's combat & movement figures
  state :: u -> UnitState
  
type Name   = String

data UnitType = Artillery 
              | Armoured
              | Infantry
              | Flak
                deriving (Eq, Show, Ord, Read)
                         
data CombatUnit = CU Name UnitState UnitType
                deriving (Eq, Show, Read)

data HQType = Division 
            | Regiment
                deriving (Eq, Show, Ord, Read)
              
data HQ = HQ Name UnitState (Maybe HQ)
        deriving (Eq, Show, Read)

