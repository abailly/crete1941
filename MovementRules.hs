-- |Contain game rules regulating movement
-- See section 6 of Crete 1941 rulebook
module MovementRules(module Common,
                     module Units, 
                     module Terrain,
                     movementCost,
                     move) where
import Common
import Units
import Terrain
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Control.Applicative(Applicative, (<$>))
import Control.Monad.State

class Order o where
  execute :: (BattleMap m t) => o -> m o
  
data Move = Move { movedUnit :: Unit,
                   fromZone  :: Zone,
                   toZone    :: Zone,
                   costs     :: Int }
            deriving (Eq, Show, Read)
                     
instance Order Move where
  execute m@(Move u from to c) = updateMovedUnit u to >> return m

-- |Moves a unit to the given named zone.
-- This function updates the terrain so that the unit's location
-- will be its destination zone, iff the unit has enough movement 
-- points to go to the location. As it updates some state, this function
-- works inside a state monad. It returns the unit (possibly modified if 
-- some MPs were consumed).
move :: (BattleMap m t) => Unit -> Name -> m Unit
move u dest = do terrain <- get
                 src <- whereIs (unitName u)
                 let [from,to] = map (zone terrain) [src, dest]
                 tryMovingUnit u from to >>= liftM movedUnit . execute

-- |Try moving a unit from a start to a destination zone.
-- Returns the zone, from or to, where the unit can move to given its current state
-- and the cost in MPs of this move (maybe 0).
tryMovingUnit :: (BattleMap m t) => Unit -> Zone -> Zone -> m Move
tryMovingUnit u from to = do t <- get 
                             let cost = movementCost u from to t
                             return $ case cost of 
                               Nothing -> Move u from from 0
                               Just v  -> if moveCapacity u >= v then Move (unitMovesBy u v) from to v else  Move u from from 0
                                                         
 
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
  | otherwise                                          = movementCost' z2

movementCost' (Zone _ _ ts Flat)     = return 1
movementCost' (Zone _ _ ts Rough)    = return 1
movementCost' (Zone _ _ ts Hilly)    = return 2
movementCost' (Zone _ _ ts Mountain) = return 4
