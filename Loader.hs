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

import           Control.Exception    (catch)
import           System.Exit
import           System.IO
import           System.Process

import           Network.Socket

import           Control.Concurrent
import           Control.Exception
import           Control.Monad        (filterM)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           Debug.Trace          (trace)
import           System.FilePath

import           Loader.Communication
import           Loader.FilesMonitor
import           Loader.Recompile

defaultLoaderPort :: Int
defaultLoaderPort = 13579

-- |Reloader configuration and current status for monitoring one application
data HotReloader = HotReloader {
  loaderFilter      ::  (FilePath -> Bool),  -- ^Filter applied to found files, only matching files are monitored
  reloadDelay       :: Int,                  -- ^Delay between each reloading attempt, in microseconds
  scanStatus        :: HashMap,              -- ^Status of scanned files
  runCount          :: Int,                  -- ^Stop reloader after this number of runs
  supervising       :: Supervised,
  runningSupervisor :: Maybe Supervisor
  }

mkReloaderConfig :: (FilePath -> Bool) -> String -> FilePath -> [String] -> Int -> Int ->
                    HotReloader
mkReloaderConfig flt main root args count port = HotReloader flt 5000000 M.empty count (Supervised "reloaded" root main args Nothing (Just port) (Just "127.0.0.1") ) Nothing

startReloader :: Int -> HotReloader -> IO HotReloader
startReloader port h = do
  s <- supervisor port ".reloader.log"
  return $ h { runningSupervisor = Just s }

-- | Kill and relaunch given application
killAndRelaunch :: HotReloader -> IO ()
killAndRelaunch c | runCount c == 0 = stopWithNickName "reloaded" (fromJust $ runningSupervisor c) >> return ()
killAndRelaunch c | runCount c /= 0 = do
  (what,state') <- recompile c
  let c' = c { scanStatus = state' , runCount = runCount c - 1 }
  case what of
    Nothing       -> threadDelay (reloadDelay c)  >> killAndRelaunch c'
    Just (ex,out) -> stopWithNickName "reloaded" (fromJust $ runningSupervisor c') >>
                     case ex of
                       ExitSuccess -> do
                         putStrLn out
                         putStr $ "Process " ++ (mainModule.supervising $ c)  ++ " succcesfully recompiled, restarting..."
                         (s',pid) <- supervise (supervising c') (fromJust $ runningSupervisor c')
                         putStrLn $ "Process restarted"
                         killAndRelaunch $ c' { runningSupervisor = Just s'}
                       _           -> do
                         putStrLn $ "Failed to recompile process " ++ (mainModule.supervising $ c) ++ ", giving up.\n" ++ out
                         killAndRelaunch c'
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


