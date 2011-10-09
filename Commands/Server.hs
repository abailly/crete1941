{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Commands.Server where

import Prelude hiding (null)
import Data.ByteString.UTF8 (fromString,toString)
import Data.ByteString(hPutStrLn,hGet,hGetLine,null)
import System.IO(Handle, hSetBuffering,
                 hFlush,IOMode(..),BufferMode(..),hClose,hIsClosed)
import Control.Applicative
import Control.Exception(finally)
import Network.Socket
import Control.Concurrent(forkIO,myThreadId, ThreadId,putMVar,MVar,newEmptyMVar,takeMVar)
import Network.BSD

import Control.Arrow(second)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Text.Regex.Posix

import MovementRules
import Terrain
import CommandsInterpreter
import Commands.IO

import qualified Text.JSON.Generic as JG

data ServerStatus = 
  Started |
  Stopped 
  deriving (Eq,Show,Ord)
           
data Server = Server { 
  threadId     :: Maybe ThreadId,
  serverStatus :: ServerStatus
  } deriving (Eq,Show)
             
makeServer :: Server
makeServer = Server Nothing Stopped

-- |Start a server for given battle map, listening on port and messaging on given variable.
startServer :: (BattleMap t) => t 
               -> Int       -- ^Server listening port
               -> MVar ()   -- ^Channel for notifying server termination
               -> IO Server -- ^A server object which can be used to control the underlying instance
startServer terrain port mvar = 
  do putStrLn $ "starting server on "  ++ (show port)
     let server = makeServer
     startup terrain port mvar server

startup :: (BattleMap t) => t 
               -> Int       -- ^Server listening port
               -> MVar ()   -- ^Channel for notifying server termination
               -> Server 
               -> IO Server
startup terrain port  mvar server = do 
  sync <- newEmptyMVar
  tid <- forkIO ((startListening terrain port sync) 
                 `finally` 
                 (putStrLn "stopping server" >> putMVar mvar ()))
  st <- takeMVar sync
  return $ server { threadId = Just tid, serverStatus = st }
  
startListening :: (BattleMap t) => t 
               -> Int       -- ^Server listening port
               -> MVar ServerStatus
               -> IO ()
startListening terrain port sync =
  do address <- inet_addr "127.0.0.1"
     sock <- socket AF_INET Stream defaultProtocol
     let addr = SockAddrInet ((fromIntegral port) :: PortNumber) address
     setSocketOption sock ReuseAddr 1
     bindSocket sock addr
     listen sock 5
     putStrLn $ "listening on " ++ show sock
     putMVar sync Started
     loopOn terrain sock
     
loopOn terrain sock = 
  do (client, clientAddress) <- accept sock
     putStrLn $ "connecting " ++ show clientAddress
     forkIO (commandsLoop client terrain)
     loopOn terrain sock

commandsLoop sock t = do hdl <- socketToHandle sock ReadWriteMode 
                         hSetBuffering hdl (LineBuffering)
                         doInterpret hdl t
                           where
                             doInterpret hdl t = ((runReaderT.runHandle) ((execStateT.runCommands) interpret t) hdl) >>= 
                                                 (\t -> do isClosed <- hIsClosed hdl 
                                                           if isClosed then return () else doInterpret hdl t)
                      
-- Low-level I/O part

newtype CommandHandleIO a = CommandHandleIO { runHandle :: ReaderT Handle IO a }
                            deriving (Monad,MonadIO, MonadReader Handle)
                                     
instance CommandIO (CommandHandleIO) where
   readCommand   = do r <- ask
                      line <- liftIO $ hGetLine r
                      liftIO (hGetLine r >>= globToEmptyLine r)
                      case  ((toString line) =~ "GET /(.*) HTTP/1.1" :: (String,String,String,[String])) of
                        (_,_,_,["units/locations"]) -> return GetUnitLocations
                        (_,_,_,["units/status"])    -> return GetUnitStatus
                        (_,_,_,_)    -> return $ CommandError ("Don't understand request "++ (toString line))
     where
       globToEmptyLine r l | l == (fromString "\r") = return ()
                           | otherwise              = hGetLine r >>= globToEmptyLine r 

   writeResult (Msg str) = ask >>= (httpReply $ unlines str)
   writeResult r         = ask >>= (httpReply $ JG.encodeJSON r)
   writeMessage msg      = ask >>= (httpReply msg)
   doExit                = ask >>= liftIO . hClose
                           
httpReply out = liftIO . flip hPutStrLn 
                (fromString $ 
                "HTTP/1.1 200 OK\r\n" ++ 
                "Content-Length: "++ (show $ length out) ++ "\r\n" ++ 
                "Content-Type: text/plain\r\n" ++ "\r\n" ++ out)
                
