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
                      
commandResultIs cmd res = (S.execState ((S.evalStateT . runCommands) interpret terrain)) ([cmd],[]) ~?= ([],[res])

commandsHandling = test [
   "commands interpreter" `should` [
       GetUnitLocations                `commandResultIs` (UnitLocations $ sort unitToLocations),
       GetUnitStatus                   `commandResultIs` (UnitStatus $ unitToStatus),
       (MoveUnit "arm1" "Beach")       `commandResultIs` (UnitMoved "arm1" "Beach"),
       (MoveUnit "arm1" "Country") `commandResultIs` (MoveProhibited "arm1" "Country")
      ]
   ]