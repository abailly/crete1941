module TestData where
import MovementRules
import Terrain.Simple
import qualified Data.Map as M


germanHQ    = Unit "hq1" German Full (UnitState 0 3 8)  DivisionHQ Nothing
britishHQ    = Unit "Campbell" British Full (UnitState 0 3 8)  DivisionHQ Nothing
germanArm   = Unit "arm1" German Full (UnitState 0 3 1)  Armoured Nothing
rethymnon   = Zone "Rethymnon" Unoccupied [City,Port] Flat
beach       = Zone "Beach" Unoccupied [Clear,Port,Beach] Flat
roughWithRoad       = Zone "Rough" Unoccupied [Clear, Road] Rough
roughNoRoad       = Zone "RoughNoRoad" Unoccupied [Clear] Rough
countryside = Zone "Country" Unoccupied [Wood] Hilly
roadCountry = Zone "CountryRoad" Unoccupied [Wood] Hilly
mountain    = Zone "Mountain" Unoccupied [Wood] Mountain
strategicZone    = Zone "A" Unoccupied [Strategic] Mountain
nonAdjacentZone = Zone "Other" Unoccupied [Clear] Flat
unitToLocations = [("Campbell", "Rethymnon"), 
                   ("hq1", "Beach"),
                   ("arm1", "Rethymnon")]
terrain     = Theater [("Rethymnon",[("Beach", True),
                                     ("Country", False),
                                     ("Rough", False),
                                     ("Country", False),
                                     ("CountryRoad", True),
                                     ("Mountain", False),
                                     ("A", False)])]
              (M.fromList unitToLocations) 
              (M.fromList [("Rethymnon", rethymnon), 
                           ("Beach", beach), 
                           ("CountryRoad",roadCountry),
                           ("Country",countryside)])
              (M.fromList [("Campbell", britishHQ), 
                           ("hq1", germanHQ),
                           ("arm1", germanArm)]) 

britishControlled (Zone n _ t l) = Zone n (Occupied (Left British)) t l
germanControlled (Zone n _ t l)  = Zone n (Occupied (Left German)) t l
contested (Zone n _ t l)         = Zone n (Occupied (Right ())) t l
