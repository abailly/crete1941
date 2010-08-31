{-# LANGUAGE FlexibleInstances,  MultiParamTypeClasses #-}
module CliTest where

import MovementRules
import Terrain.Simple
import CommandsInterpreter
import qualified Control.Monad.State as S
import Control.Monad.Reader
import Test.HUnit
import Test.QuickCheck
import TestUtilities
import TestData
import Data.List

-- |An instance of CommandIO for test purpose
-- fst contains command to read 
-- snd contains output from interpreter
instance CommandIO (S.State ([Command],[CommandResult])) where
   readCommand   = do ((c:cs),rs) <- S.get
                      S.put (cs,rs)
                      return c
   writeResult r = do (cs,rs) <- S.get
                      S.put (cs, r:rs)
                      return ()
                      
checkCommandResultFor cmd res = (S.execState ((S.evalStateT . runCommands) interpret terrain)) ([cmd],[]) ~?= ([],[res])

commandsHandling = test [
   "commands interpreter" `should` [
      ("display (unit,zone) couples as unit locations" `for`
       checkCommandResultFor GetUnitLocations (UnitLocations $ sort unitToLocations)),
      ("display (unit,state) couples as unit status" `for`
       checkCommandResultFor GetUnitStatus (UnitStatus $ unitToStatus))
      ]
   ]