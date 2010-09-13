module Main where
import CommandsInterpreter
import Commands.IO
import TestData
import AllTests
import System.Exit
import System.IO
import System
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

main = do args <- getArgs
          case args of
            ["test"] -> runAllTests
            _        -> interpreterLoop terrain

interpreterLoop t =  (execStateT.runCommands) interpret t >>= interpreterLoop