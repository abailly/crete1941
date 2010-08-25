module Units where
import Common

data UnitState = UnitState { offense :: Int,
                             defense :: Int,
                             movement :: Int }
                 deriving (Eq, Show, Read)
                          
data State = Full 
           | Reduced 
           | Disorganized
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

