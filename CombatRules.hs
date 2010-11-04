module CombatRules where
import Common
import Units
import Terrain

type Force = (Unit,  -- Main attacking unit
              Zone,  -- Zone unit is located in
              Int)   -- Dice
             
-- |Possible atomic outcomes of a combat
-- A combat between two or more units may yield a varied set of outcomes.
data CombatOutcome = Reduce Unit    -- ^Unit is reduced 
                   | Eliminate Unit -- ^Unit is automatically eliminated
                   | Retreat Unit   -- ^Unit must retreat to adjacent zone
                   deriving (Eq, Show)
                                 
-- |Reduce a unit to half its strength
reduce :: Unit -> Unit
reduce u@(Unit _ _ _ (UnitState off def mov) _ _) = u { unitState = Reduced, unitStrength  = UnitState (off `quot` 2) def mov }

assaultOutcome :: Force -> Force -> [CombatOutcome]
assaultOutcome (att,_,adice) (def,_,ddice) | attack att + adice > 2 * (defend def + ddice)  = [Reduce def, Retreat def]
                                           | attack att + adice > defend def + ddice        = [Reduce def]

fireOutcome :: Force -> Force -> [CombatOutcome]
fireOutcome (att,_,adice) (def,_,ddice) | attack att + adice > 2 * (defend def + ddice)  = [Eliminate def]
                                        | attack att + adice > defend def + ddice        = [Reduce def]
                                        | attack att + adice == defend def + ddice       = [Reduce def, Reduce att]
                                        | otherwise                                      = [Reduce att]

