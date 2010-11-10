module CombatRules where
import Common
import Units
import Terrain
import Data.Maybe(fromJust)
import Control.Arrow
import Control.Monad.State(get)

tryAssault :: (BattleMap t) => Name -> Name -> Battle t (Maybe (Unit,[Unit],Zone,Unit,[Unit],Zone))
tryAssault attName defName  =  do t <- get
                                  azone <- whereIs attName
                                  let attacker  = unit attName t
                                  let allies = filter (\u -> (unitSide u == unitSide attacker) && (u /= attacker)) $ map (flip unit t) $ unitsIn t azone
                                  dzone <- whereIs defName
                                  return $ if azone == dzone 
                                           then Just (attacker,allies,zone t azone,unit defName t,[],zone t dzone)
                                           else Nothing

tryFire :: (BattleMap t) => Name -> Name -> Battle t (Maybe (Unit,[Unit],Zone,Unit,[Unit],Zone))
tryFire attName defName  = do t <- get
                              azone <- whereIs attName
                              dzone <- whereIs defName
                              return $ if adjacent t azone dzone 
                                       then Just (unit attName t,[],zone t azone,unit defName t,[],zone t dzone)
                                       else Nothing

type Force = (Unit,   -- Main attacking unit
              [Unit], -- Supporting units
              Zone,   -- Zone unit is located in
              Int)    -- Dice
             
-- |Possible atomic outcomes of a combat
-- A combat between two or more units may yield a varied set of outcomes.
data CombatOutcome = Reduce Unit    -- ^Unit is reduced 
                   | Eliminate Unit -- ^Unit is automatically eliminated
                   | Retreat Unit   -- ^Unit must retreat to adjacent zone
                   deriving (Eq)
                                 
instance Show CombatOutcome where
  show (Reduce u)    = "Reduce " ++ (unitName u)
  show (Eliminate u) = "Eliminate " ++ (unitName u)
  show (Retreat u)   = "Retreat " ++ (unitName u)
  
-- |Apply CombatOutcome
combatOutcome :: (BattleMap t) => CombatOutcome -> Battle t CombatOutcome
combatOutcome o@(Reduce u) 
  | unitState u == Reduced  = eliminate u >> return o 
  | otherwise               = updateStatusOf (reduce u) >> return o
combatOutcome o@(Eliminate u) = eliminate u >> return o 


-- |Reduce a unit to half its strength
reduce :: Unit -> Unit
reduce u@(Unit _ _ _ (UnitState off def mov) _ _) = u { unitState = Reduced, unitStrength  = UnitState (off `quot` 2) def mov }

(-->) = fireOutcome
(##>) = assaultOutcome

assaultOutcome :: Force -> Force -> [CombatOutcome]
assaultOutcome fa@(att,support,_,adice) fd@(def,_,_,ddice) | netAttackerStrength fa > 2 * (netDefenderStrength fd)   = [Reduce def, Retreat def]
                                                           | netAttackerStrength fa > netDefenderStrength fd         = [Reduce def]
                                                           | netAttackerStrength fa == netDefenderStrength fd        = [Reduce def, Retreat def, Reduce att, Retreat att]
                                                           | 2 * (netAttackerStrength fa) < (netDefenderStrength fd) = [Reduce att, Retreat att] ++ map Retreat support
                                                           | otherwise                                               = [Reduce att, Retreat att]

fireOutcome :: Force -> Force -> [CombatOutcome]
fireOutcome fa@(att,_,_,adice) fd@(def,_,_,ddice) | netAttackerStrength fa > 2 * (netDefenderStrength fd)  = [Eliminate def]
                                                  | netAttackerStrength fa > netDefenderStrength fd        = [Reduce def]
                                                  | netAttackerStrength fa == netDefenderStrength fd       = [Reduce def, Reduce att]
                                                  | otherwise                                              = [Reduce att]

netDefenderStrength :: Force -> Int
netDefenderStrength (def,support,zone,ddice) = defend def + 
                                               colocatedWithHq def support +
                                               terrainBonus zone +
                                               ddice

terrainBonus :: Zone -> Int
terrainBonus zone = sum (map snd $                          -- extract bonus
                         filter fst $                       -- select effects that apply
                         map (($zone) *** id) terrainEffect -- compute terrain effect predicate
                        ) 

-- |All terrain effects that can possibly apply. 
-- Terrain effect is expressed as a predicate in order to be cumulative
terrainEffect :: [(Zone -> Bool, Int)]
terrainEffect = [
  ((== Rough) . zoneScape,              1),
  ((== Hilly) . zoneScape,              2),
  ((elem Wood) . zoneTerrainAttributes, 1)
  ]

netAttackerStrength :: Force -> Int 
netAttackerStrength (att,support,_,adice) = attack att + 
                                            length support + 
                                            groupedGermanRegiment (att:support) +
                                            colocatedWithHq att support +
                                            adice 

colocatedWithHq :: Unit -> [Unit] -> Int 
colocatedWithHq unit support = if unitHq unit /= Nothing && fromJust (unitHq unit) `elem` support then 1 else 0
  
groupedGermanRegiment :: [Unit] -> Int
groupedGermanRegiment units = if countSameRegiment units >= 3 then 1 else 0

countSameRegiment :: [Unit] -> Int
countSameRegiment []  = 0
countSameRegiment [u] = 1
countSameRegiment (u:u':us) | u `sameRegiment` u'  = 1 + countSameRegiment (u':us)
                            | otherwise            = max (countSameRegiment (u:us)) (countSameRegiment (u':us))


