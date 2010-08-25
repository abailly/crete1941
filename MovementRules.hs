-- |Contain game elements definitions: zones, units, charts...
module MovementRules(module Common,
                     module Units, 
                     module Terrain,
                     movementCost) where
import Common
import Units
import Terrain
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Control.Applicative(Applicative, (<$>))


-- |Gives the movement cost for a unit moving from given start zone
-- to given target zone on given terrain. 

movementCost :: (Applicative m, Monad m, Terrain t) => t -> Unit -> Zone -> Zone -> m Integer
movementCost t u@(Unit _ s _ _ _ _) z1 z2@(Zone _ o _ _ ) = (occupationFactor s o) <$> baseMovementCost t u z1 z2
  
occupationFactor :: Side -> Control -> (Integer -> Integer)
occupationFactor s (Occupied (Left s')) | s /= s' = (+1)
occupationFactor _ (Occupied (Right ()))          = (+1)
occupationFactor _ _                              = (+0)

baseMovementCost :: (Monad m, Terrain t) => t -> Unit -> Zone -> Zone -> m Integer
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
