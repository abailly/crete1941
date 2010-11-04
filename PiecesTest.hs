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

spendMpsFrom unit zone (target,expected)  = ("MPs expense from " ++ show (zoneName target)  ++ " are " ++ show expected) ~: 
                                            movementCost unit zone target terrain @?= expected

mpsForHQFromRethymnonIs         target expected = movementCost germanHQ  rethymnon target terrain @?= expected
mpsForBritishHQFromRethymnonIs  target expected = movementCost britishHQ rethymnon target terrain @?= expected
mpsForMechFromRethymnonIs       target expected = movementCost germanArm rethymnon target terrain @?= expected
matchingLocationForUnitInTerrain (name, zone)   = (name ++ " is in " ++ show zone) ~: unitLocation name terrain @?= zone  

unitManipulations = test [
  "update data of unit" `should` [
     "allow relative update of movement points" `for`
     quickCheck (\ i -> moveCapacity (unitMovesBy germanHQ i) == moveCapacity germanHQ - i)
     ]
  ]
                    
movementRules = test [
  "cost of movement for units" `should` (
  germanHQ `spendMpsFrom` rethymnon `with` 
  [
    (beach           , Just 1),
    (roughWithRoad   , Just 1),
    (countryside     , Just 2),
    (mountain        , Just 4),
    (strategicZone   , Just 6),
    (roadCountry     , Just 1),
    (nonAdjacentZone , Nothing)
  ]),
    
  "cost of movement to controlled & contested zones" `should` [
    germanHQ `spendMpsFrom` rethymnon $ (britishControlled roughWithRoad,Just 2),
    germanHQ `spendMpsFrom` rethymnon $ (contested mountain, Just 5),
    britishHQ `spendMpsFrom` rethymnon $ (germanControlled roadCountry, Just 2),
    britishHQ `spendMpsFrom` rethymnon $ (contested mountain, Just 5)
    ],
  
  "cost of movement for mechanized units" `should` ( 
  germanArm `spendMpsFrom` rethymnon `with`
  [
    (roughWithRoad,Just 1),
    (roughNoRoad,Nothing)
  ]),
  
  "unit initial location" `should` (matchingLocationForUnitInTerrain `with` unitToLocations),  
  
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
  "one infantry unit firing another in adjacent clear zones" `should` [
     "reduce defender when outcome is favorable to attacker" `for`
     fireOutcome (germanI100,beach,6) (nz22ndBat,beach,5)    ~?= [Reduce nz22ndBat],
     "eliminate defender when outcome is very favorable to attacker" `for`
     fireOutcome (germanI100,beach,5) (greek1stRgt,beach,2)  ~?= [Eliminate greek1stRgt],
     "reduce value by 2 when outcome is favorable to defender" `for`
     fireOutcome (germanI100,beach,5) (nz22ndBat,beach,7)    ~?= [Reduce germanI100],
     "reduce both parties' when outcome is draw" `for`
     fireOutcome (germanI100,beach,6) (nz22ndBat,beach,7)    ~?= [Reduce nz22ndBat, Reduce germanI100]
     ],
  
  "one infantry unit assaulting another in a zone" `should` [
    "reduce defender and retreat when outcome is very favorable to attacker" `for`
    assaultOutcome (germanI100,beach,5) (greek1stRgt,beach,2)  ~?= [Reduce greek1stRgt, Retreat greek1stRgt],
    "reduce defender when outcome is favorable to attacker" `for`
    assaultOutcome (germanI100,beach,6) (nz22ndBat,beach,5)    ~?= [Reduce nz22ndBat]
    ]
  ]
