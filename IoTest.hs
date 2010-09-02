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
    ("GetUnitLocations", GetUnitLocations),
    ("GetUnitStatus", GetUnitStatus),
    ("getunitStatus", GetUnitStatus),
    ("move arm1 rethymnon", MoveUnit "arm1" "rethymnon")
  ]
  ]