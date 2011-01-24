module Loader.Recompile where
import System.FilePath(FilePath)
import System.Process(waitForProcess,CreateProcess(..),createProcess,proc,StdStream(CreatePipe))
import System.Exit(ExitCode)
import Control.Concurrent(takeMVar,newEmptyMVar,putMVar,forkIO)
import Control.Exception(evaluate)
import System.IO(hClose,hGetContents)


doRecompile :: String -> FilePath -> IO (ExitCode,String)
doRecompile mainModule rootDirectory = do
  (_, Just outh, Just errh, pid) <-
       createProcess (proc "D:/Program Files/Haskell Platform/2010.2.0.0/bin/ghc.exe" 
                      ["--make", "-threaded", mainModule]) { cwd = Just rootDirectory,
                                                             std_out = CreatePipe, 
                                                             std_err = CreatePipe}
  outMVar <- newEmptyMVar
  
  -- fork off a thread to start consuming stdout
  out  <- hGetContents outh
  _ <- forkIO $ evaluate (length out) >> putMVar outMVar ()
  
  -- fork off a thread to start consuming stderr
  err  <- hGetContents errh
  _ <- forkIO $ evaluate (length err) >> putMVar outMVar ()
  
  -- wait both threads' end
  takeMVar outMVar
  takeMVar outMVar
  hClose outh
  -- wait on the process
  ex <- waitForProcess pid
  return (ex,out ++ err)
