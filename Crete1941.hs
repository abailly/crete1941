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
            ["test"]   -> withSocketsDo $ do mvar <- newEmptyMVar
                                             startServer terrain (fromIntegral serverPort)  mvar          
                                             runAllTests 
                                             putMVar mvar ()
                                             return ExitSuccess
            ["server"] -> withSocketsDo $ do mvar <- newEmptyMVar
                                             startServer terrain (fromIntegral serverPort)  mvar          
                                             takeMVar mvar
                                             return ExitSuccess
            _          -> interpreterLoop terrain

interpreterLoop t =  (execStateT.runCommands) interpret t >>= interpreterLoop

