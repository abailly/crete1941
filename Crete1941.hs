module Main where
import CommandsInterpreter
import Commands.IO
import TestData
import System.Exit
import System.IO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

main = interpreterLoop terrain

interpreterLoop t =  (execStateT.runCommands) interpret t >>= interpreterLoop