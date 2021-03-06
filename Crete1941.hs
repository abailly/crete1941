module Main where
import CommandsInterpreter
import Commands.IO
import TestData
import AllTests
import System.Exit
import System.IO
import System.Directory
import Network
import System
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Concurrent

import Commands.Server
import Loader.Communication

main = do args <- getArgs
          dir <- getCurrentDirectory
          case args of 
            ["wtest"] -> withSocketsDo $ do mvar <- newEmptyMVar
                                            server <- startWarpServer terrain (fromIntegral serverPort) mvar (Just dir)
                                            runAllTests  tests
                                            runAllTests  testsWithServer
                                            stopWarpServer server
                                            return ExitSuccess
            _        -> withSocketsDo $ do mvar <- newEmptyMVar
                                           startWarpServer terrain (fromIntegral serverPort)  mvar (Just dir)
                                           runAllTests tests
                                           runAllTests  testsWithServer
                                           takeMVar mvar
                                           return ExitSuccess


interpreterLoop t =  do c <- readCommand
                        let (r,t) = (runState (executeCommand c) t)
                        writeResult r
                        interpreterLoop t

