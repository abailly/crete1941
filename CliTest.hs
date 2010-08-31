{-# LANGUAGE FlexibleInstances,  MultiParamTypeClasses #-}
module CliTest where

import MovementRules
import Terrain.Simple
import CommandsInterpreter
import Control.Monad.State
import Control.Monad.Reader
import Test.HUnit
import Test.QuickCheck
import TestUtilities
import TestData
import Data.List

instance CommandIO [] where
   readCommand                   = [GetUnitLocations]
   writeResult (UnitLocations u) = [()]
  
commandsHandling = test [
   "commands interpreter" `should` [
      ("display (unit,zone) couples as unit locations" `for`
       ((evalStateT . runCommands) interpret terrain) ~?= [UnitLocations $ sort unitToLocations])
      ]
   ]