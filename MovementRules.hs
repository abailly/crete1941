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

-- |Moves a unit to the given named zone.
-- This function updates the terrain so that the unit's location
-- will be its destination zone, iff the unit has enough movement 
-- points to go to the location. As it updates some state, this function
-- works inside a state monad and returns the arrival zone.
move :: (Terrain t, BattleMap m t) => Unit -> Name -> m Zone
move u dest = do terrain <- get
                 src <- whereIs (unitName u)
                 let [from,to] = map (zone terrain) [src, dest]
                 let res = if unitCanMoveTo terrain u from to  then to else from
                 updateUnitPosition (unitName u) (zoneName res)
                 return res

-- |Select whether a unit can move to a given zone, given a terrain
-- and a starting zone.
unitCanMoveTo :: (Terrain t) => t -> Unit -> Zone -> Zone -> Bool
unitCanMoveTo terrain u from to = 
  case movementCost terrain u from to  of
    Nothing -> False
    Just v  -> movement (unitStrength u) >= v 
                 
 
-- |Gives the movement cost for a unit moving from given start zone
-- to given target zone on given terrain. 
movementCost :: (Applicative m, Monad m, Terrain t) => t -> Unit -> Zone -> Zone -> m Int
movementCost t u@(Unit _ s _ _ _ _) z1 z2@(Zone _ o _ _ ) = (occupationFactor s o) <$> baseMovementCost t u z1 z2
  
occupationFactor :: Side -> Control -> (Int -> Int)
occupationFactor s (Occupied (Left s')) | s /= s' = (+1)
occupationFactor _ (Occupied (Right ()))          = (+1)
occupationFactor _ _                              = (+0)

baseMovementCost :: (Monad m, Terrain t) => t -> Unit -> Zone -> Zone -> m Int
baseMovementCost terrain unit z1@(Zone n1 _ _ _ ) z2@(Zone n2 _ ts _) 
  | not $ adjacent terrain n1 n2                       = fail "non adjacent target terrain"
  | Strategic `elem` ts                                = return 6
  | isConnectedByRoadTo terrain n1 n2                  = return 1
  | unitType unit == Armoured && not (Road `elem` ts)  = fail "armoured unit moving into non road zone"
  | otherwise                                          = movementCost' z2

movementCost' (Zone _ _ ts Flat)     = return 1
movementCost' (Zone _ _ ts Rough)    = return 1
movementCost' (Zone _ _ ts Hilly)    = return 2
movementCost' (Zone _ _ ts Mountain) = return 4
