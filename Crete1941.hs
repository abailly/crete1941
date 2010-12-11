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
          withSocketsDo $ do mvar <- newEmptyMVar
                             startServer terrain (fromIntegral serverPort)  mvar          
                             runAllTests 
                             takeMVar mvar
                             return ExitSuccess

interpreterLoop t =  (execStateT.runCommands) interpret t >>= interpreterLoop

