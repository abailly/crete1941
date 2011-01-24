{-
A code reloader.

This loader does the following:
 - scans the source files for potential changes,
 - compile the source files using GHC's make,
 - kill an existing instance of the program, if any, by sending a HUP
   signal. This should have the additional side-effect that the
   program saves its state,
 - restart the program by loading it through GHC in another process
   and invoking its main function.
-}

module Loader where
import System
import System.IO
import System.IO.Error(catch)
import System.Process
import System.Exit

import Network.Socket
import Network.Multicast

import Control.Exception
import Control.Concurrent
import Control.Monad(filterM)
import qualified Data.Map as M
import System.FilePath
import Debug.Trace(trace)

import Loader.Communication
import Loader.FilesMonitor
import Loader.Recompile

defaultLoaderPort :: Int
defaultLoaderPort = 13579

-- |Reloader configuration and current status for monitoring one application
data HotReloader = HotReloader {
  loaderFilter    ::  (FilePath -> Bool),  -- ^Filter applied to found files, only matching files are monitored
  reloadDelay     :: Int,                  -- ^Delay between each reloading attempt, in microseconds
  scanStatus      :: HashMap,              -- ^Status of scanned files 
  runCount        :: Int,                  -- ^Stop reloader after this number of runs
  supervising     :: Supervisor
  }
                    
mkReloaderConfig :: (FilePath -> Bool) -> String -> FilePath -> [String] -> Int -> Int ->  
                    HotReloader
mkReloaderConfig flt main root args count port = HotReloader flt 5000000 M.empty count (Supervised root main args Nothing Nothing)

-- | Kill and relaunch given application
killAndRelaunch :: HotReloader -> Maybe ProcessHandle -> IO ()
killAndRelaunch c maybePid | runCount c == 0 = stopWithPid maybePid (signalPort.supervising $ c)
killAndRelaunch c maybePid | runCount c /= 0 = do
  (what,state') <- recompile c
  let c' = c { scanStatus = state' , runCount = runCount c - 1 } 
  case what of
    Nothing       -> threadDelay (reloadDelay c)  >> killAndRelaunch c' maybePid
    Just (ex,out) -> stopWithPid maybePid (signalPort.supervising $ c') >> 
                     case ex of
                       ExitSuccess -> do
                         putStrLn out
                         putStr $ "Process " ++ (mainModule.supervising $ c)  ++ " succcesfully recompiled, restarting..."
                         pid' <- supervise (supervising c')
                         putStrLn $ "Process restarted"
                         killAndRelaunch c' (Just pid')
                       _           -> do 
                         putStrLn $ "Failed to recompile process " ++ (mainModule.supervising $ c) ++ ", giving up.\n" ++ out
                         killAndRelaunch c' Nothing
  where
    
-- | Recompile application if there is any change in the underlying
-- source files.
recompile :: HotReloader ->                       -- ^Root directory to scan changes in
             IO (Maybe (ExitCode,String),HashMap) -- ^If application has been recompiled, log the result, otherwise Nothing
recompile c = do
  (changes,state') <- checkChanges (loaderFilter c) [(rootDirectory.supervising $  c)] (scanStatus c)
  if changes /= [] then  
    doRecompile (mainModule.supervising $  c) (rootDirectory.supervising $  c) >>= \log -> return (Just log, state')  
    else    
    return (Nothing, state')
    

