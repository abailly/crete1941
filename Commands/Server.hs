{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, Rank2Types #-}
module Commands.Server(startWarpServer, application) where

import Data.ByteString.UTF8 (fromString,toString)
import Control.Exception(finally)
import Control.Concurrent(forkIO,myThreadId, ThreadId,putMVar,MVar,newEmptyMVar,takeMVar)

import Data.Enumerator(Iteratee)

import Control.Concurrent(threadDelay)
import Control.Concurrent.STM

import System.IO.Unsafe

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

import Yesod.Logger

import Text.Printf(printf)

data ServerStatus = 
  Started |
  Stopped 
  deriving (Eq,Show,Ord)
                
-- WAI-based low-level I/O

data (BattleMap t, Show t) => Server t = Server {
  threadId      :: Maybe ThreadId,
  serverStatus  :: ServerStatus,
  sharedSession :: TVar t,
  logger        :: Logger
  }
                                         
instance (BattleMap t, Show t) => Show (Server t) where
  show server = printf "Server { threadId = %s, serverStatus = %s, sharedSession = %s }"
                (show $ threadId server)
                (show $ serverStatus server)
                (show $ unsafePerformIO $ readTVarIO $ sharedSession server)
  
                   

startWarpServer :: (BattleMap t, Show t) => t 
               -> Int       -- ^Server listening port
               -> MVar ()   -- ^Channel for notifying server termination
               -> IO (Server t) -- ^A server object which can be used to control the underlying instance
startWarpServer t port sync = do 
  ref <- newTVarIO t
  l  <- makeLogger
  let server = Server Nothing Stopped ref l
  timed l (T.pack $ "starting server on "  ++ (show port)) (spawnWarpServer port sync server) 
   
                     
spawnWarpServer :: (BattleMap t, Show t) => Int       -- ^Server listening port
               -> MVar ()   -- ^Channel for notifying server termination
               -> Server t 
               -> IO (Server t)
spawnWarpServer port mvar server = do 
  tid <- forkIO ((run port $ application (logger server) (sharedSession server))
                 `finally` 
                 (putStrLn "stopping server" >> putMVar mvar ()))
  return $ server { threadId = Just tid, serverStatus = Started }
  
application :: (BattleMap t, Show t) => Logger -> TVar t -> Application
application l ref r = do 
  timed l (T.pack $ "serving request " ++ show r) (
    case map T.unpack $ pathInfo r of
      ["exit"]               -> action ref Exit
      ["units","locations"]  -> action ref GetUnitLocations
      ["units","status"]     -> action ref GetUnitStatus
      ["unit",name]          -> action ref (SingleUnitStatus name)
      ["unit",name,"move"]   -> action ref (decodeMove (queryString r) name Nothing)
      _                      -> return $ respond ("Don't understand request "++ (B8.unpack $ rawPathInfo r)) )

decodeMove :: Query -> String -> Maybe String -> Command
decodeMove []                  unit (Just to) = MoveUnit unit to
decodeMove []                  unit  _         = CommandError "moving unit need to define target zone"
decodeMove ((par,Nothing):xs)  unit  p      = decodeMove xs unit p
decodeMove ((par,Just val):xs) unit  _     = case B8.unpack par of 
  "to"   -> decodeMove xs unit (Just (B8.unpack val))

action ::  (BattleMap t, Show t) => TVar t -> Command -> Data.Enumerator.Iteratee B8.ByteString IO Response
action ref act = do  
  result <- lift $ atomically $ do 
    t <- readTVar ref
    let (r,t') = runState (executeCommand act) t
    writeTVar ref t'
    return r
  return $ respond result
    
respond r         = responseLBS statusOK [(mk $ B8.pack "Content-Type", B8.pack "application/json")] (LB8.pack $ JG.encodeJSON r)
