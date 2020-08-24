module Main where
import           AllTests
import           Commands.IO
import           CommandsInterpreter
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import           Network
import           System.Environment
import           System.Exit
import           System.IO
import           TestData

import           Commands.Server
import           Loader.Communication

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


interpreterLoop t =  (execStateT.runCommands) interpret t >>= interpreterLoop

