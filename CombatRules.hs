module CombatRules where
import Common
import Units
import Terrain

type Force = (Unit,  -- Main attacking unit
              Zone,  -- Zone unit is located in
              Int)   -- Dice
             
newtype CombatOutcome = CombatOutcome [Unit]
                        deriving (Eq, Show)
                                 
-- |Reduce a unit to half its strength
reduce :: Unit -> Unit
reduce u@(Unit _ _ _ (UnitState off def mov) _ _) = u { unitState = Reduced, unitStrength  = UnitState (off `quot` 2) def mov }

fireOutcome :: Force -> Force -> CombatOutcome
fireOutcome (att,_,adice) (def,_,ddice) | attack att + adice > defend def + ddice  = CombatOutcome [reduce def]
                                        | attack att + adice == defend def + ddice = CombatOutcome [reduce def, reduce att]
  where
    attack = offense . unitStrength
    defend = defense . unitStrength