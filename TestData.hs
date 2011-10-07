module TestData where
import MovementRules
import Terrain.Simple
import qualified Data.Map as M

-- for testing
germanHQ        = Unit "hq1" German Full (UnitState 0 3 8)  DivisionHQ Nothing
britishHQ       = Unit "Campbell" British Full (UnitState 0 3 8)  DivisionHQ Nothing
britishInf      = Unit "Campbell" British Full (UnitState 0 3 0)  Infantry (Just britishHQ)
germanArm       = Unit "arm1" German Full (UnitState 0 3 1)  Armoured Nothing

-- german roster
gjDiv5          = Unit "HQ GebirsJager Div 5" German Full (UnitState 0 3 6) DivisionHQ Nothing
gj100Rgt        = Unit "HQ GJ 100 Rgt" German Full (UnitState 0 3 6) RegimentHQ (Just gjDiv5)
germanI100      = Unit "I/100 Rgt" German Full (UnitState 4 3 6)  Infantry (Just gj100Rgt)
germanII100     = Unit "II/100 Rgt" German Full (UnitState 4 3 6)  Infantry (Just gj100Rgt)
germanIII100    = Unit "III/100 Rgt" German Full (UnitState 4 3 6)  Infantry (Just gj100Rgt)
gj85Rgt         = Unit "HQ GJ 85 Rgt" German Full (UnitState 0 3 6) RegimentHQ (Just gjDiv5)
germanI85       = Unit "I/85 Rgt" German Full (UnitState 4 3 6)  Infantry (Just gj85Rgt)

-- british roster
hq5Brigade      = Unit "HQ 5th Brigade" British Full (UnitState 3 3 0) RegimentHQ Nothing
greek1stRgt     = Unit "Greek 1st Reg" British Full (UnitState 2 2 4) Infantry (Just hq5Brigade)
nz21stBat       = Unit "NZ 21st Bat" British Full (UnitState 2 3 6) Infantry (Just hq5Brigade)
nz22ndBat       = Unit "NZ 22nd Bat" British Full (UnitState 4 3 6) Infantry (Just hq5Brigade)

rethymnon       = Zone "Rethymnon" Unoccupied [City,Port] Flat
beach           = Zone "Beach" Unoccupied [Clear,Port,Beach] Flat
roughWithRoad   = Zone "Rough" Unoccupied [Clear, Road] Rough
roughNoRoad     = Zone "RoughNoRoad" Unoccupied [Clear] Rough
woody           = Zone "Woody" Unoccupied [Wood] Flat
hilly           = Zone "Hilly" Unoccupied [Clear] Hilly
countryside     = Zone "Country" Unoccupied [Wood] Hilly
roadCountry     = Zone "CountryRoad" Unoccupied [Wood] Hilly
mountain        = Zone "Mountain" Unoccupied [Wood] Mountain
strategicZone   = Zone "A" Unoccupied [Strategic] Mountain
nonAdjacentZone = Zone "Other" Unoccupied [Clear] Flat

unitToLocations = [("Campbell", "Rethymnon"), 
                   ("hq1", "Beach"),
                   ("arm1", "Rethymnon"),
                   ("inf1", "Rethymnon"),
                   ("I/100 Rgt", "Beach"),
                   ("II/100 Rgt", "Hilly"),
                   ("III/100 Rgt", "CountryRoad"),
                   ("I/85 Rgt", "CountryRoad"),
                   ("NZ 22nd Bat", "Rethymnon"),
                   ("Greek 1st Reg", "CountryRoad"),
                   ("NZ 21st Bat", "Hilly")
                   ]
unitToStatus      = [("Campbell", britishHQ),
                     ("arm1", germanArm), 
                     ("hq1", germanHQ),
                     ("inf1",britishInf),
                     ("I/100 Rgt", germanI100),
                     ("II/100 Rgt", germanII100),
                     ("III/100 Rgt", germanIII100),
                     ("I/85 Rgt", gj85Rgt),
                     ("Greek 1st Reg", greek1stRgt),
                     ("NZ 22nd Bat",nz22ndBat),
                     ("NZ 21st Bat",nz21stBat)]
zoneToStatus      = [("Rethymnon", rethymnon), 
                     ("Beach", beach), 
                     ("Rough", roughWithRoad), 
                     ("CountryRoad",roadCountry),
                     ("Country",countryside),
                     ("Hilly",hilly)]
terrain     = Theater [("Rethymnon",[("Beach", True),
                                     ("Country", False),
                                     ("Rough", False),
                                     ("Country", False),
                                     ("CountryRoad", True),
                                     ("Mountain", False),
                                     ("A", False)]),
                       ("Beach",[("Rethymnon", True),
                                 ("Rough", False)]),
                       ("Hilly",[])]
              (M.fromList unitToLocations) 
              (M.fromList zoneToStatus)
              (M.fromList unitToStatus)
              (alternatingDices 42)

alternatingDices seed = firstDie : alternatingDices secondDie
  where
    firstDie = (seed `rem` 10 + 2)
    secondDie = 14 - firstDie
    
britishControlled (Zone n _ t l) = Zone n (Occupied (Left British)) t l
germanControlled (Zone n _ t l)  = Zone n (Occupied (Left German)) t l
contested (Zone n _ t l)         = Zone n (Occupied (Right ())) t l

serverPort = 5678