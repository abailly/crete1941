module Main where
import CommandsInterpreter
import Commands.IO
import TestData
import System.Exit
import System.IO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

instance CommandIO IO where
  readCommand = do input <- getLine 
                   return $ decode input
  writeResult r = putStrLn $ show r
    
main = forever $ ((runStateT.runCommands) interpret terrain) 
