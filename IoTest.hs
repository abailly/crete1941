-- |Test low-level I/O layer for interacting with user
module IoTest where

import CommandsInterpreter
import Commands.IO
import Test.HUnit
import Test.QuickCheck
import TestUtilities
import TestData

readSimpleCommandsFromStrings (s,c) = 
  decode s @?= c
  
decodeCommandsFromStrings = test [
  "commands decoding" `shouldBe` 
  readSimpleCommandsFromStrings `with` 
  [
    ("GetUnitLocations", Right GetUnitLocations),
    ("GetUnitStatus", Right GetUnitStatus),
    ("getunitStatus", Right GetUnitStatus),
    ("move arm1 rethymnon", Right $MoveUnit "arm1" "rethymnon"),
    ("exit", Right Exit),
    ("unknown command", Left "unknown command")
  ]
  ]