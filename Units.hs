{-# LANGUAGE DeriveDataTypeable #-}
module Units where
import Common
import qualified Data.Generics as G


data UnitState = UnitState { offense :: Int,
                             defense :: Int,
                             movement :: Int }
                 deriving (Eq, Show, Read, G.Data, G.Typeable)
                          
data State = Full 
           | Reduced 
           | Disorganized
           deriving (Eq, Show, Read, G.Data, G.Typeable)
                    
data UnitType = Artillery 
              | Armoured
              | Infantry
              | Flak
              | DivisionHQ
              | RegimentHQ
                deriving (Eq, Show, Ord, Read, G.Data, G.Typeable)
                         
data Unit = Unit { unitName     :: Name, 
                   unitSide     :: Side, 
                   unitState    :: State, 
                   unitStrength :: UnitState, 
                   unitType     :: UnitType, 
                   unitHq       :: (Maybe Unit) }
          deriving (Eq, Show, Read, G.Data, G.Typeable)

-- | Decrease the movement capacity of the unit by the 
-- given amount
unitMovesBy :: Unit -> Int -> Unit
unitMovesBy (Unit n s st (UnitState o d m) t h) c =  
  (Unit n s st (UnitState o d (m - c)) t h) 
  
moveCapacity :: Unit -> Int
moveCapacity (Unit n s st (UnitState o d m) t h) = m

-- |Compare unit by their names
compareUnitsBy :: (Ord a) => (Unit -> a) -> Unit -> Unit -> Ordering
compareUnitsBy f u1 u2 = compare (f u1) (f u2)

-- |Attack strength of unit
attack = offense . unitStrength

-- |Defense strength of unit
defend = defense . unitStrength
