{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Commands.Server where

import           Control.Applicative
import           Control.Arrow         (second)
import           Control.Concurrent    (MVar, ThreadId, forkIO, myThreadId,
                                        putMVar)
import           Control.Exception     (finally)
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString       (hGet, hGetLine, null)
import           Data.ByteString.Char8 (hPutStrLn)
import           Data.ByteString.UTF8  (fromString, toString)
import           Network.BSD
import           Network.Socket
import           Prelude               hiding (null)
import           System.IO             (BufferMode (..), Handle, IOMode (..),
                                        hClose, hFlush, hIsClosed,
                                        hSetBuffering)
import           Text.Regex.TDFA

import           Commands.IO
import           CommandsInterpreter
import           MovementRules
import           Terrain

import qualified Text.JSON.Generic     as JG

newtype CommandHandleIO a = CommandHandleIO { runHandle :: ReaderT Handle IO a }
                            deriving (Functor,Applicative,Monad,MonadIO, MonadReader Handle)

instance CommandIO (CommandHandleIO) where
   readCommand   = do r <- ask
                      liftIO $ putStrLn ("try reading input from " ++ (show r))
                      line <- liftIO $ hGetLine r
                      liftIO $ putStrLn ("read input from " ++ (show r) ++  ": " ++ (show line))
                      liftIO (hGetLine r >>= globToEmptyLine r)
                      case  ((toString line) =~ "GET /(.*) HTTP/1.1" :: (String,String,String,[String])) of
                        (_,_,_,[])  -> liftIO (putStrLn $ "fail match "++ (toString line)) >> (return $ CommandError ("Don't understand request "++ (toString line)))
                        (_,_,_,[x]) -> case decode (x) of
                                         Right c -> liftIO (putStrLn $ "match "++ x) >> return c
                                         Left  m -> liftIO (putStrLn $ "unknown command "++ x) >> (return $ CommandError m)
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

startServer :: (BattleMap t) => t -> Int -> MVar () -> IO ThreadId
startServer terrain port mvar =
  do putStrLn $ "starting server on "  ++ (show port)
     forkIO ((startListening terrain port) `finally` (putStrLn "stopping server" >> putMVar mvar ()))

startListening terrain port =
  do address <- inet_addr "127.0.0.1"
     sock <- socket AF_INET Stream defaultProtocol
     let addr = SockAddrInet ((fromIntegral port) :: PortNumber) address
     bindSocket sock addr
     listen sock 5
     putStrLn $ "listening on " ++ show sock
     loopOn terrain sock

loopOn terrain sock =
  do (client, clientAddress) <- accept sock
     putStrLn $ "connecting " ++ show client
     forkIO (commandsLoop client terrain)
     loopOn terrain sock

commandsLoop sock t = do hdl <- socketToHandle sock ReadWriteMode
                         hSetBuffering hdl (LineBuffering)
                         doInterpret hdl t
                           where
                             doInterpret hdl t = ((runReaderT.runHandle) ((execStateT.runCommands) interpret t) hdl) >>=
                                                 (\t -> do isClosed <- hIsClosed hdl
                                                           if isClosed then return () else doInterpret hdl t)

