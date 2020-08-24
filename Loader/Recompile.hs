module Loader.Recompile where
import           Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception  (evaluate)
import           System.Exit        (ExitCode)
import           System.FilePath    (FilePath)
import           System.IO          (hClose, hGetContents)
import           System.Process     (CreateProcess (..), StdStream (CreatePipe),
                                     createProcess, proc, waitForProcess)


doRecompile :: String -> FilePath -> IO (ExitCode,String)
doRecompile mainModule rootDirectory = do
  (_, Just outh, Just errh, pid) <-
       createProcess (proc "/usr/local/bin/ghc"
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
