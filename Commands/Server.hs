{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Commands.Server where

import Prelude hiding (null)
import Data.ByteString.UTF8 (fromString,toString)
import Data.ByteString(hPutStrLn,hGet,hGetLine,null)
import System.IO(Handle, hSetBuffering,
                 hFlush,IOMode(..),BufferMode(..),hClose)
import Control.Applicative
import Control.Exception(finally)
import Network.Socket
import Control.Concurrent(forkIO,ThreadId,putMVar,MVar)
import Network.BSD
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import MovementRules
import Terrain
import CommandsInterpreter
import Commands.IO
import Text.Regex.TDFA

newtype CommandHandleIO a = CommandHandleIO { runHandle :: ReaderT Handle IO a }
                            deriving (Monad,MonadIO, MonadReader Handle)
                                     
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

   writeResult (Msg str) = ask >>= (liftIO . flip hPutStrLn (fromString $ unlines str))
   writeResult r         = ask >>= (liftIO . flip hPutStrLn (fromString $ show r))
   writeMessage msg      = ask >>= (liftIO . flip hPutStrLn (fromString msg))
   doExit                = ask >>= liftIO . hClose
       
startServer :: (BattleMap t) => t -> Int -> MVar () -> IO ThreadId
startServer terrain port mvar = 
  do putStrLn $ "starting server on "  ++ (show port) 
     forkIO ((doServe terrain port) `finally` (putStrLn "stopping server" >> putMVar mvar ()))
  
doServe terrain port =
  do putStrLn "do start server"
     address <- inet_addr "127.0.0.1"
     sock <- socket AF_INET Stream defaultProtocol
     let addr = SockAddrInet ((fromIntegral port) :: PortNumber) address
     bindSocket sock addr
     listen sock 5
     putStrLn $ "listening " ++ show sock
     loopOn terrain sock
     
loopOn terrain sock = 
  do (client, clientAddress) <- accept sock
     putStrLn $ "connecting " ++ show client
     forkIO (commandsLoop client terrain `finally` sClose client)
     loopOn terrain sock

commandsLoop sock t = do hdl <- socketToHandle sock ReadWriteMode 
                         hSetBuffering hdl (LineBuffering)
--                       req <- hGetLine hdl
--                       putStrLn (toString req)
                         ((runReaderT.runHandle) ((execStateT.runCommands) interpret t) hdl) >>= commandsLoop sock

