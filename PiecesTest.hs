module PiecesTest where
import Test.HUnit
import Test.QuickCheck
import Control.Monad.State(runState,execState,evalState)
import qualified Data.Map as M
import Control.Arrow
import TestUtilities(when, for,should,with, given)

import MovementRules
import CombatRules
import Terrain.Simple
import TestData

mpsExpendedFrom unit zone (target,expected)  = ("MPs expense from " ++ show (zoneName target)  ++ " are " ++ show expected) ~: 
                                            movementCost unit zone target terrain @?= expected

mpsForHQFromRethymnonIs         target expected = movementCost germanHQ  rethymnon target terrain @?= expected
mpsForBritishHQFromRethymnonIs  target expected = movementCost britishHQ rethymnon target terrain @?= expected
mpsForMechFromRethymnonIs       target expected = movementCost germanArm rethymnon target terrain @?= expected
matchingLocationForUnitInTerrain (name, zone)   = (name ++ " is in " ++ show zone) ~: unitLocation name terrain @?= zone  

germanRgtInClear dice = (germanI100,[],beach,dice)
britRgtInClear dice   = (nz22ndBat,[],beach,dice)
reduceAndRetreatAttacker = [Reduce germanI100,Retreat germanI100]
retreatSupport = [Retreat germanII100]
reduceAndRetreatDefender = [Reduce nz22ndBat, Retreat nz22ndBat]
reduceDefender = [Reduce nz22ndBat]

unitManipulations = test [
  "update data of unit" `should` [
     "allow relative update of movement points" `for`
     quickCheck (\ i -> moveCapacity (unitMovesBy germanHQ i) == moveCapacity germanHQ - i)
     ]
  ]
                    
movementRules = "movement rules" ~: test [
  "cost of movement for units" `should` (
  germanHQ `mpsExpendedFrom` rethymnon `with` 
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
    germanHQ `mpsExpendedFrom` rethymnon $ (britishControlled roughWithRoad,Just 2),
    germanHQ `mpsExpendedFrom` rethymnon $ (contested mountain, Just 5),
    britishHQ `mpsExpendedFrom` rethymnon $ (germanControlled roadCountry, Just 2),
    britishHQ `mpsExpendedFrom` rethymnon $ (contested mountain, Just 5)
    ],
  
  "cost of movement for mechanized units" `should` ( 
  germanArm `mpsExpendedFrom` rethymnon `with`
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
                
combatRules = "combat rules" ~: test [
  
  given "more than one unit assaults a single unit in a zone" [
    "supporting unit ads +1 to attacker" `for` 
    (germanI100,[germanII100],beach,6) ##> (britRgtInClear 7)                       ~?= reduceDefender,
    "supporting units retreat when outcome is very favorable to defender" `for` 
    (germanI100,[germanII100],beach,2) ##> (britRgtInClear 12)                      ~?= reduceAndRetreatAttacker ++ retreatSupport,
    "3 batallions adds +1 to attacker" `for` 
    (germanI100,[germanII100,germanII100],beach,4) ##> (britRgtInClear 7)           ~?= reduceDefender,
    "3 batallions adds +1 to attacker" `for` 
    (germanI100,[germanII100,germanI85,germanII100],beach,3) ##> (britRgtInClear 7) ~?= reduceDefender,
    "own HQ adds +1 to attacker" `for` 
    (nz22ndBat,[hq5Brigade],beach,3) ##> (germanRgtInClear 5)                       ~?= [Reduce germanI100]
    ],
  
  given "one unit attacking a defender in some special situation"  [
    "own HQ adds +1 to defender" `when`
    (germanRgtInClear 5) ##> (nz22ndBat,[hq5Brigade],beach,6)       ~?= reduceAndRetreatAttacker,
    "wood terrain adds +1" `for` 
    (germanRgtInClear 5) ##> (nz22ndBat,[hq5Brigade],woody,5)       ~?= reduceAndRetreatAttacker,
    "rough terrain adds +1" `for` 
    (germanRgtInClear 5) ##> (nz22ndBat,[hq5Brigade],roughNoRoad,5) ~?= reduceAndRetreatAttacker,
    "hill terrain adds +2" `for` 
    (germanRgtInClear 5) ##> (nz22ndBat,[hq5Brigade],hilly,4)       ~?= reduceAndRetreatAttacker,
    "terrain bonus is cumulative" `for` 
    (germanRgtInClear 5) ##> (nz22ndBat,[],countryside,4)           ~?= reduceAndRetreatAttacker
    ],
  
  given "one unit tries to attack a defender" [
    "when units are not in same zone, assault is prohibited" `for`
    eval (tryAssault "I/100 Rgt" "NZ 22nd Bat") ~?= Nothing,
    "when units are in adjacent zones, fire is permited" `for`
    eval (tryFire    "I/100 Rgt" "NZ 22nd Bat") ~?= Just (germanI100,[],beach,nz22ndBat,[],rethymnon),
    "when unit are not in adjacent zones, fire is prohibited" `for`
    eval (tryFire    "I/100 Rgt" "NZ 21st Bat") ~?= Nothing,
    "when units are in same zone, assault is permitted" `for`
    eval (tryAssault "II/100 Rgt" "NZ 21st Bat") ~?= Just (germanII100,[],hilly,nz21stBat,[],hilly),
    "colocated units contribute to attack for" `for`
    eval (tryAssault "III/100 Rgt" "Greek 1st Reg") ~?= Just (germanIII100,[gj85Rgt],roadCountry,greek1stRgt,[],roadCountry)
    ],
  
  given "a unit engages another" [
    "combat order is issued when engaging (and dices are thrown)" `for`
    eval (engage "II/100 Rgt" "NZ 21st Bat" ) ~?= (germanII100,[],hilly,4) :##> (nz21stBat,[],hilly,2),
    "no combat order is issued when assaulting is not possible" `for`
    eval (engage "II/100 Rgt" "NZ 22nd Bat" ) ~?= NoCombat,
    "fire combat order is issued when assaulting is not possible but fire is possible" `for`
    eval (engage "I/100 Rgt" "NZ 22nd Bat" ) ~?= (germanI100,[],beach,4) :--> (nz22ndBat,[],rethymnon,2)
    ],
  
  given "a combat order is issued" [
    "battle map is updated with combat outcome and outcome has old unit status" `for`
    ((attack.changedUnit.head) *** (attack.unit "NZ 21st Bat")) (run (combat "II/100 Rgt" "NZ 21st Bat")) ~?= (2,1)
    ]
  ]
  where
    eval = flip (evalState.runBattle) terrain
    run  = flip (runState.runBattle) terrain

combatEffect = given "some combat outcome"  [
  "When reducing unit then it halves its attack strength" `for`
  attack (unit "I/100 Rgt" $ runCombat (Reduce germanI100))  ~?= 2,
  "When eliminated, unit disappears from battle" `for`
  "I/100 Rgt" `elem` map fst (allUnitStatus $ runCombat (Eliminate germanI100))  ~?= False ,
  "When reducing unit then it set its state to reduced" `for`
  unitState (unit "I/100 Rgt" $ runCombat (Reduce germanI100))  ~?= Reduced,
  "When reducing reduced unit then it its eliminated" `for`
  "I/100 Rgt" `elem` map fst (allUnitStatus $ runCombat (Reduce $ reduce germanI100))  ~?= False
  ]
  where
    runCombat = flip (execState.runBattle) terrain . combatOutcome