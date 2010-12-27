{-# LANGUAGE ScopedTypeVariables #-}
-- |A communication layer for distributed processes wanting to exchange 
-- events about their lifecycle. This layer allows a Loader supervisor
-- to send and receive events from other processes, and dispatch control 
-- messages.
module Loader.Communication where
import Network.Socket
import Control.Exception
import Control.Concurrent(threadDelay)
import System.Process(terminateProcess)



stopWithPid (Just pid) port = sendStopSignal port >> 
                              threadDelay 1000000 >> 
                              try (terminateProcess pid) >>=
                              either 
                                (\ (e :: IOException) -> putStrLn ("Cannot terminate process: '" ++ (show e) ++ "', contnuing....") >> return ())
                                (\ _ -> return ())
stopWithPid Nothing    port = return ()
    
sendStopSignal :: Int -> IO Int
sendStopSignal port = withSocketsDo $ do
  address <- inet_addr "127.0.0.1"
  sock <- socket AF_INET Datagram defaultProtocol
  let addr = SockAddrInet ((fromIntegral port) :: PortNumber) address
  sendTo sock "stop" addr
  
