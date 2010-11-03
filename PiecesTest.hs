module PiecesTest where
import Test.HUnit
import Test.QuickCheck
import Control.Monad.State
import qualified Data.Map as M
import Control.Arrow
import Control.Monad
import TestUtilities

import MovementRules
import CombatRules
import Terrain.Simple
import TestData

mpsForHQFromRethymnonIs         target expected = movementCost germanHQ rethymnon target terrain @?= expected
mpsForBritishHQFromRethymnonIs  target expected = movementCost britishHQ rethymnon target terrain @?= expected
mpsForMechFromRethymnonIs       target expected = movementCost germanArm rethymnon target terrain @?= expected
matchingLocationForUnitInTerrain (name, zone) = unitLocation name terrain  @?= zone  

unitManipulations = test [
  "update data of unit" `should` [
     "allow relative update of movement points" `for`
     quickCheck (\ i -> moveCapacity (unitMovesBy germanHQ i) == moveCapacity germanHQ - i)
     ]
  ]
                    
movementRules = test [
  "cost of movement for units" `shouldBe` 
  uncurry mpsForHQFromRethymnonIs `with` 
  [
    (beach           , Just 1),
    (roughWithRoad   , Just 1),
    (countryside     , Just 2),
    (mountain        , Just 4),
    (strategicZone   , Just 6),
    (roadCountry     , Just 1),
    (nonAdjacentZone , Nothing)
  ],
    
  "cost of movement to controlled & contested zones" `shouldBe` [
    britishControlled roughWithRoad `mpsForHQFromRethymnonIs` Just 2,
    contested mountain `mpsForHQFromRethymnonIs` Just 5,
    germanControlled roadCountry `mpsForBritishHQFromRethymnonIs` Just 2,
    contested mountain `mpsForBritishHQFromRethymnonIs` Just 5
    ],
  
  "cost of movement for mechanized units" `shouldBe`  uncurry mpsForMechFromRethymnonIs `with`
  [
    (roughWithRoad,Just 1),
    (roughNoRoad,Nothing)
  ],
  
  "unit initial location" `shouldBe` matchingLocationForUnitInTerrain `with` unitToLocations,
  
  "moving unit to target zone" `should` [
    "change unit's location in terrain if it has enough MPs and change its MPs" `for`
    ((movement . unitStrength . movedUnit) *** unitLocation "Campbell") ((runState . runBattle) (move "Campbell" "Beach") terrain)  ~?= (7,"Beach"),
    "don't change unit's location in terrain it it cannot move to target" `for`
    unitLocation "arm1"  ((execState  . runBattle) (move "arm1" "Country") terrain)  ~?= "Rethymnon",
    "don't change unit's location in terrain it does not have enough MPs" `for`
    unitLocation "inf1"  ((execState  . runBattle) (move "inf1" "Beach") terrain)  ~?= "Rethymnon",
    "store updated status of unit when it has moved" `for`
    moveCapacity (unit "Campbell" $ (execState . runBattle) (move "Campbell" "Country") terrain)  ~?= 6
    ]
  ]
                
combatRules = test [
  "one infantry unit firing another in adjacent zone" `should` [
     "divide defender's value by 2 when outcome is very favorable to attacker" `for`
     fireOutcome (germanI100,beach,12) (greek1stRgt,beach,2) ~?= CombatOutcome [reduce greek1stRgt],
     "divide defender's value by 2 when outcome is favorable to attacker" `for`
     fireOutcome (germanI100,beach,6) (nz22ndBat,beach,5)    ~?= CombatOutcome [reduce nz22ndBat],
     "divide both parties' value by 2 when outcome is draw" `for`
     fireOutcome (germanI100,beach,6) (nz22ndBat,beach,7)    ~?= CombatOutcome [reduce nz22ndBat, reduce germanI100]
     ] 
  ]