{-# LANGUAGE FlexibleInstances,  MultiParamTypeClasses, TypeSynonymInstances #-}
module CliTest where

import MovementRules
import Terrain.Simple
import CommandsInterpreter
import Commands.IO
import qualified Control.Monad.State as S
import Control.Monad.Reader
import Test.HUnit
import Test.QuickCheck
import TestUtilities
import TestData
import Data.List
import Data.Ord (comparing)

interpretOneCommandWith cmd= (S.evalState (executeCommand cmd) terrain)

commandResultIs cmd res = (show cmd) ++ " yields " ++ (take 30 (show res)) ++ "..." ~: interpretOneCommandWith cmd  ~?= res
 
commandsHandling = test [
   "Interpreting command" `should` [
       GetUnitLocations                `commandResultIs` (UnitLocations $ sort unitToLocations),
       GetUnitStatus                   `commandResultIs` (UnitStatus $ sortBy (comparing fst) unitToStatus),
       (MoveUnit "arm1" "Beach")       `commandResultIs` (UnitMoved "arm1" "Beach"),
       (MoveUnit "arm1" "Country")     `commandResultIs` (MoveProhibited "arm1" "Country"),
       CommandError "error"            `commandResultIs` (ErrorInCommands "unknown command: error"),
       Exit                            `commandResultIs` Bye
      ]
   ,
   "command execution" `should` [
     "change terrain when moving unit" `for`
     (S.evalState (executeCommand (MoveUnit "Campbell" "Beach") >> S.get >>= (return . unitLocation "Campbell") ) terrain) ~?= "Beach"
     ]
   ]
