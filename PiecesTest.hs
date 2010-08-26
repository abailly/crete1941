module PiecesTest where
import Test.HUnit
import MovementRules
import Terrain.Simple
import Control.Monad.State
import qualified Data.Map as M

should n = (n ~:) . TestList  
for    n = (n ~:)

infixl 0 `for`

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
terrain     = Theater [("Rethymnon",[("Beach", True),
                                     ("Country", False),
                                     ("Rough", False),
                                     ("Country", False),
                                     ("CountryRoad", True),
                                     ("Mountain", False),
                                     ("A", False)])]
              (M.fromList [("Campbell", "Rethymnon"), 
                           ("hq1", "Beach"),
                           ("arm1", "Rethymnon")]) 
              (M.fromList [("Rethymnon", rethymnon), 
                           ("Beach", beach), 
                           ("CountryRoad",roadCountry),
                           ("Country",countryside)])

britishControlled (Zone n _ t l) = Zone n (Occupied (Left British)) t l
germanControlled (Zone n _ t l)  = Zone n (Occupied (Left German)) t l
contested (Zone n _ t l)         = Zone n (Occupied (Right ())) t l

mpsForHQFromRethymnonIs target expected = movementCost terrain germanHQ rethymnon target ~?= expected
mpsForBritishHQFromRethymnonIs target expected = movementCost terrain britishHQ rethymnon target ~?= expected
mpsForMechFromRethymnonIs target expected = movementCost terrain germanHQ rethymnon target ~?= expected
  
movementRules = test [
  "cost of movement for units" `should` [
     beach           `mpsForHQFromRethymnonIs` Just 1,
     roughWithRoad   `mpsForHQFromRethymnonIs` Just 1,
     countryside     `mpsForHQFromRethymnonIs` Just 2,
     mountain        `mpsForHQFromRethymnonIs` Just 4,
     strategicZone   `mpsForHQFromRethymnonIs` Just 6,
     roadCountry     `mpsForHQFromRethymnonIs` Just 1,    
     nonAdjacentZone `mpsForHQFromRethymnonIs` Nothing
     ],
  
  "cost of movement to controlled & contested zones" `should` [
    britishControlled roughWithRoad `mpsForHQFromRethymnonIs` Just 2,
    germanControlled roadCountry `mpsForBritishHQFromRethymnonIs` Just 2,
    contested mountain `mpsForHQFromRethymnonIs` Just 5,
    contested mountain `mpsForBritishHQFromRethymnonIs` Just 5
    ],
  
  "cost of movement for mechanized units" `should` [
    roughWithRoad `mpsForMechFromRethymnonIs` (Just 1),
    roughNoRoad   `mpsForMechFromRethymnonIs` Nothing
    ],
  
  "unit initial location" `should` [
    "be drawn from terrain definition" `for` TestList [
       unitLocation "Campbell" terrain  ~?= "Rethymnon",
       unitLocation "hq1" terrain ~?= "Beach"
       ]
    ],
  
  "moving unit to target zone" `should` [
    "change unit's location in terrain if it has enough MPs" `for`
    unitLocation "Campbell" (execState (move britishHQ "Beach") terrain)  ~?= "Beach",
    "don't change unit's location in terrain it it has not enough MPs" `for`
    unitLocation "arm1" (execState (move germanArm "Country") terrain)  ~?= "Rethymnon"
    ]
  ]
                


