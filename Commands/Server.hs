module Commands.Server(startWarpServer, 
                       stopWarpServer) where

import Data.ByteString.UTF8 (fromString,toString)
import Control.Exception(finally)
import Control.Concurrent(forkIO,myThreadId, ThreadId,putMVar,MVar,newEmptyMVar,takeMVar)

import Data.Enumerator(Iteratee)
import Data.List(intersperse)

import Control.Concurrent(threadDelay, killThread)
import Control.Concurrent.STM

import System.IO.Unsafe
import System.FilePath

import Control.Monad.Trans(lift)
import Control.Monad.State(runState)

import MovementRules
import Terrain
import CommandsInterpreter

import qualified Text.JSON.Generic as JG

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.Text as T 
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.CaseInsensitive(mk)

import Commands.Log

import Debug.Trace

import Data.Time

import Text.Printf(printf)

data ServerStatus = 
  Started |
  Stopped 
  deriving (Eq,Show,Ord)
                
data (BattleMap t, Show t) => Server t = Server {
  threadId      :: Maybe ThreadId,
  serverStatus  :: ServerStatus,
  sharedSession :: TVar t,
  logger        :: Logger,
  port          :: Int,
  started       :: UTCTime,
  baseDirectory :: FilePath
  }
                                         
instance (BattleMap t, Show t) => Loggable (Server t) where
  getLogger = logger 
  getStartTime = started
  getIdentifier s = "localhost@" ++ (show $ port s)
  
instance (BattleMap t, Show t) => Show (Server t) where
  show server = printf "Server { threadId = %s, serverStatus = %s, sharedSession = %s }"
                (show $ threadId server)
                (show $ serverStatus server)
                (show $ unsafePerformIO $ readTVarIO $ sharedSession server)
                   
startWarpServer ::  (BattleMap t, Show t) => t 
               -> Int           -- ^Server listening port
               -> MVar ()       -- ^Channel for notifying server termination
               -> Maybe FilePath      -- ^Base directory the server runs in 
               -> IO (Server t) -- ^A server object which can be used to control the underlying instance
startWarpServer t port sync basedir = do 
  ref <- newTVarIO t
  l  <- makeLogger
  start <- getCurrentTime
  let server = Server Nothing Stopped ref l port start (maybe "." id basedir)
  notifyServerEvent server ServerStart
  spawnWarpServer port sync server
                     
stopWarpServer :: (BattleMap t, Show t) => Server t -> IO (Server t)
stopWarpServer s@(Server (Just tid) Started _ l port start dir) = do
  notifyServerEvent s ServerStop
  flushLogger l 
  killThread tid
  return s { threadId = Nothing, serverStatus = Stopped }

spawnWarpServer :: (BattleMap t, Show t) => Int       -- ^Server listening port
               -> MVar ()   -- ^Channel for notifying server termination
               -> Server t 
               -> IO (Server t)
spawnWarpServer port mvar server = do 
  tid <- forkIO ((run port $ application server)
                 `finally` 
                 (putMVar mvar ()))
  return $ server { threadId = Just tid, serverStatus = Started }
  
application :: (BattleMap t, Show t) => Server t -> Application
application s r = do 
  doExchange s r 
    (case routing r of
      (GET,  ("resources":path))     -> tryServeFile s (concat $ intersperse "/" path)
      (POST, ["exit"])               -> action ref Exit
      (GET,  ["units","locations"])  -> action ref GetUnitLocations
      (GET,  ["units","status"])     -> action ref GetUnitStatus
      (GET,  ["unit",name])          -> action ref (SingleUnitStatus name)
      (POST, ["unit",name,"move"])   -> action ref (extractParameter "to" (queryString r)  (MoveUnit name))
      (POST, ["unit",name,"attack"]) -> action ref (extractParameter "tgt" (queryString r) (Attack name ))
      unknown                        -> return $ respond statusNotFound ("Don't understand request "++ (B8.unpack $ rawPathInfo r)))
 where
   ref = sharedSession s
   routing :: Request -> (StdMethod, [String])
   routing (Request method _ _ _ _ _ _ _ _ pinfo _) = (either 
                                                       (\t -> error $ "incorrect method " ++ (show t)) id 
                                                       (parseMethod method), 
                                                       map T.unpack pinfo)
   
tryServeFile server path = do
  let file = (baseDirectory server </> path)
  return $ ResponseFile statusOK [(mk $ B8.pack "Content-Type", B8.pack (inferContentType file))] file Nothing

inferContentType file =  case splitExtension file of 
        (_,".jpg")  -> "image/jpeg"
        (_,".png")  -> "image/png"
        (_,".txt")  -> "text/plain"
        (_,".xml")  -> "application/xml"
        (_,".html") -> "text/html"
        (_,".css")  -> "text/css"
        (_,".js")   -> "application/javascript"
        (_,".json") -> "application/json"
        (f,e)       -> "application/octet-stream" 

doExchange s req action  = do
  start <- lift $ getCurrentTime
  rep <- action
  lift $ notifyServerEvent s (HandleRequest req rep start)
  return rep

extractParameter :: String -> Query -> (String -> Command) -> Command
extractParameter param query command = case decodeParam query param Nothing of
  Nothing -> CommandError $ "cannot execute command, missing parameter " ++ param ++ " in " ++ (show query)
  Just p  -> command p
  
decodeParam :: Query -> String -> Maybe String -> Maybe String
decodeParam []                  param  Nothing  = Nothing
decodeParam ((par,Nothing):xs)  param  p        = decodeParam xs param p
decodeParam ((par,Just val):xs) param  _        = case B8.unpack par of 
  param   -> (Just (B8.unpack val))

action ::  (BattleMap t, Show t) => TVar t -> Command -> Data.Enumerator.Iteratee B8.ByteString IO Response
action ref act = do  
  result <- lift $ atomically $ do 
    t <- readTVar ref
    let (r,t') = runState (executeCommand act) t
    writeTVar ref t'
    return r
  return $ respond statusOK result
    
respond status r  = responseLBS status  [(mk $ B8.pack "Content-Type", B8.pack "application/json")] (LB8.pack $ JG.encodeJSON r)
