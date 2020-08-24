{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- |A communication layer for distributed processes wanting to exchange
-- events about their lifecycle. This layer allows a Loader supervisor
-- to send and receive events from other processes, and dispatch control
-- messages.
module Loader.Communication where
import           Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar,
                                     takeMVar, threadDelay)
import           Control.Exception  (evaluate, finally)
import           Data.IORef
import           Data.List          (find, intercalate)
import           Data.Maybe         (fromJust)
import           Data.Time
import           Network.Socket
import           Prelude            hiding (log)
import           System.FilePath    ((</>))
import           System.Process     (CreateProcess (..), ProcessHandle,
                                     createProcess, proc, terminateProcess)

-- |A Log message with a timestamp
type LogItem = (UTCTime, String)

data Supervisor = Supervisor {
  supervised  :: IORef [Supervised], -- ^All supervised process
  signalPort  :: Int,                -- ^Port this supervisor is listening on
  logFile     :: FilePath,           -- ^Filename where all supervisor actions are logged to
  history     :: [LogItem],           -- ^Buffer of all logged messages
  termination :: MVar Supervisor    -- ^Initially empty. Supervisor puts itself into this MVar whenever it terminates
  }

instance Show Supervisor where
  show (Supervisor ref port log h _ ) = "Supervisor " ++ (show port) ++ " " ++ log ++ "\n" ++ unlines (map show h)

-- |Supervised configuration.
data Supervised  = Supervised {
  nickName       :: String,               {- ^Nickname for this supervised process,
                                               must be unique across all supervised processes within a specific supervisor -}
  rootDirectory  :: FilePath,             -- ^Root directory to monitor
  mainModule     :: String,               -- ^Main module (maybe in dotted or slash notation)
  mainArgs       :: [String],             -- ^Main program arguments
  procHandle     :: Maybe ProcessHandle,  -- ^Handle to this process, not empty if process has started
  supervisedPort :: Maybe Int,            -- ^Port this supervised process is listening on
  supervisedHost :: Maybe String          -- ^Host this supervised process is running on
  }

instance Show Supervised where
  show (Supervised n r m a _ pt hs) = intercalate " " ["Supervised",n,r,m,(show a),show pt, show hs]

-- |Create and start a new Supervisor.
supervisor :: Int -> String -> IO Supervisor
supervisor port logFile = do
  sups <- newIORef []
  mv <- newEmptyMVar
  let sup = Supervisor sups port logFile [] mv
  log sup ("starting supervisor " ++ (show sup)) >>=  (withSocketsDo . forkIO . runSupervisor)
  return sup

  -- |Request supervision from supervisor to given supervised process.
supervise :: Supervised -> Supervisor -> IO (Supervisor, ProcessHandle)
supervise inf sup = do (_, _, _, pid') <-
                         createProcess (proc pathToMain (comPort sup ++ (mainArgs inf))) {cwd = Just root}
                       let inf' = inf { procHandle = Just pid' }
                       modifyIORef (supervised sup) (inf':)
                       sup' <- log sup ("started supervised process " ++ (show inf'))
                       return (sup', pid')
  where
    root       = rootDirectory inf
    pathToMain = root </> map (replace '.' '/') (mainModule inf)
    comPort c' = "127.0.0.1" : (show $ signalPort c') :[]

runSupervisor :: Supervisor -> IO ()
runSupervisor s = do sock <- startListening (signalPort s)
                     finally (loop sock s) (sClose sock)
                       where
                         loop sock s = do (msg,len,addr) <- recvFrom sock 1024
                                          if "stop" == msg
                                          then (log s "stopped") >>= stopped
                                          else (log s ("[" ++ (show addr) ++ "] " ++ msg) >>= loop sock)
                         stopped s   = putMVar (termination s) s

log s msg   = (appendFile (logFile s) $ msg  ++ "\n") >> appendLog s msg

appendLog s msg = do timestamp <- getCurrentTime
                     return $ s { history = (timestamp,msg) : history s }

stopSupervisor :: Supervisor -> IO Int
stopSupervisor sup = do
  address <- inet_addr ("127.0.0.1")
  sock <- socket AF_INET Datagram defaultProtocol
  let addr = SockAddrInet ((fromIntegral.signalPort $ sup) :: PortNumber) address
  log sup "stopping"
  sendTo sock "stop" addr

startListening :: Int -> IO Socket
startListening port = do
  address <- inet_addr "127.0.0.1"
  sock <- socket AF_INET Datagram defaultProtocol
  let addr = SockAddrInet ((fromIntegral port) :: PortNumber) address
  bindSocket sock addr
  return sock

replace :: (Eq a) => a -> a -> a -> a
replace from to x | x == from = to
                  | otherwise = x

-- |Stop process with given Nickname
stopWithNickName :: String -> Supervisor -> IO (Supervisor, Maybe Supervised)
stopWithNickName nick sup =
  sendStopSignal nick sup >>= (return . (sup,))

sendStopSignal :: String -> Supervisor -> IO (Maybe Supervised)
sendStopSignal nick sup = do
  sups <- readIORef (supervised sup)
  case find  ((== nick) . nickName) sups of
    Just inf -> do
      address <- inet_addr (fromJust.supervisedHost $ inf)
      sock <- socket AF_INET Datagram defaultProtocol
      let addr = SockAddrInet ((fromIntegral.fromJust.supervisedPort $ inf) :: PortNumber) address
      sendTo sock "stop" addr
      return $ Just inf
    Nothing -> return Nothing

