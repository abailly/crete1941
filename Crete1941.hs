module Main where
import CommandsInterpreter
import Commands.IO
import TestData
import AllTests
import System.Exit
import System.IO
import Network
import System
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Commands.Server
import Control.Concurrent

main = do args <- getArgs
          case args of 
            ["test"] -> withSocketsDo $ do mvar <- newEmptyMVar
                                           startServer terrain (fromIntegral serverPort)  mvar          
                                           runAllTests 
                                           return ExitSuccess
            _        -> withSocketsDo $ do mvar <- newEmptyMVar
                                           startServer terrain (fromIntegral serverPort)  mvar          
                                           runAllTests 
                                           takeMVar mvar
                                           return ExitSuccess

-- main loop
interpreterLoop t =  (execStateT.runCommands) interpret t >>= interpreterLoop

