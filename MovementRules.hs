{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, DeriveDataTypeable #-}
-- |Contain game rules regulating movement
-- See section 6 of Crete 1941 rulebook
module MovementRules(module Common,
                     module Units, 
                     module Terrain,
                     movementCost,
                     move,
                     prepareMove,
                     Move(..)) where
import Common
import Units
import Terrain
import Orders
import Control.Applicative(Applicative, (<$>))
import Control.Monad.State
import qualified Data.Generics as G

data Move = Move { movedUnit :: Unit,
                   fromZone  :: Zone,
                   toZone    :: Zone,
                   costs     :: Int }
          | NoMove
          deriving (Eq, Show, Read, 
                    G.Data, G.Typeable -- ^Move needs to be serializable
                   )
                     
instance Order Move where
  type Result Move = Move
  execute m@(Move u from to c) = updateMovedUnit u to >> return m
  execute m                    = return m

-- |Moves a unit to the given named zone.
-- This function updates the terrain so that the unit's location
-- will be its destination zone, iff the unit has enough movement 
-- points to go to the location. As it updates some state, this function
-- works inside a state monad. It returns the unit (possibly modified if 
-- some MPs were consumed).
move :: (BattleMap t) => Name -> Name -> Battle t Move
move uname dest = prepareMove uname dest >>= execute


-- | Prepare movement of a unit.
-- This function does not change the state of the unit or the terrain.
prepareMove :: (BattleMap t) => 
               Name                -- ^Name of the unit to move
               -> Name             -- ^Name of destination zone
               -> Battle t Move    -- ^Computed Move order that may be executed later on.
prepareMove uname dest = do terrain <- get
                            src <- whereIs uname
                            let [from,to] = map (flip zone terrain) [src, dest]
                            tryMovingUnit (unit uname terrain) from to 
                            
-- |Try moving a unit from a start to a destination zone.
-- Returns the zone, from or to, where the unit can move to given its current state
-- and the cost in MPs of this move (maybe 0).
tryMovingUnit :: (BattleMap t) => Unit -> Zone -> Zone -> Battle t Move
tryMovingUnit u from to = do t <- get 
                             let cost = movementCost u from to t
                             return $ case cost of 
                               Nothing -> NoMove
                               Just v  -> if moveCapacity u >= v then Move (unitMovesBy u v) from to v else  NoMove
                                                         
 
-- |Gives the movement cost for a unit moving from given start zone
-- to given target zone on given terrain. 
movementCost :: (Applicative m, Monad m, Terrain t) =>  Unit -> Zone -> Zone -> t -> m Int
movementCost u@(Unit _ s _ _ _ _) z1 z2@(Zone _ o _ _ ) t = (occupationFactor s o) <$> baseMovementCost u z1 z2 t
  
occupationFactor :: Side -> Control -> (Int -> Int)
occupationFactor s (Occupied (Left s')) | s /= s' = (+1)
occupationFactor _ (Occupied (Right ()))          = (+1)
occupationFactor _ _                              = (+0)

baseMovementCost :: (Monad m, Terrain t) => Unit -> Zone -> Zone -> t -> m Int
baseMovementCost unit z1@(Zone n1 _ _ _ ) z2@(Zone n2 _ ts _) terrain 
  | not $ adjacent terrain n1 n2                       = fail "non adjacent target terrain"
  | Strategic `elem` ts                                = return 6
  | isConnectedByRoadTo terrain n1 n2                  = return 1
  | unitType unit == Armoured && not (Road `elem` ts)  = fail "armoured unit moving into non road zone"
  | otherwise                                          = return $ movementCost' z2

movementCost' (Zone _ _ ts Flat)     = 1
movementCost' (Zone _ _ ts Rough)    = 1
movementCost' (Zone _ _ ts Hilly)    = 2
movementCost' (Zone _ _ ts Mountain) = 4
