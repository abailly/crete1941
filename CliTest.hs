{-# LANGUAGE FlexibleInstances,  MultiParamTypeClasses #-}
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
   doExit        = do (cs,rs) <- S.get
                      S.put (cs, rs)
                      return ()
                      
interpretOneCommandWith f cmd= (S.execState ((f . runCommands) interpret terrain)) cmd

commandResultIs cmd res = (show cmd) ++ " yields " ++ (take 30 (show res)) ++ "..." ~: interpretOneCommandWith S.evalStateT ([cmd],[])  ~?= ([],[res])
 
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
     ((S.execStateT.runCommands) (executeCommand (MoveUnit "Campbell" "Beach")) terrain) >>= (assertEqual "Unexpected location" "Beach" . unitLocation "Campbell")     
     ]
   ]