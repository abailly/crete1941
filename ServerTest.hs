-- |Tests interactions through a server.
module ServerTest where

import CommandsInterpreter
import Commands.Server
import Test.HUnit
import Test.QuickCheck
import TestUtilities
import TestData
import Text.Regex
import Network.HTTP
import Network.URI
import Data.Maybe      
import Control.Concurrent(killThread,newEmptyMVar,putMVar)

-- From http://book.realworldhaskell.org/read/extended-example-web-client-programming.html      
simpleHttpContent url =
    do putStrLn ("connecting to " ++ url)
       resp <- simpleHTTP request
       case resp of
         Left x -> return $ ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ (show r)
                   Just url -> simpleHttpContent url
               _ -> return $ (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}
          uri = fromJust $ parseURI url

matchReplyContentToRESTCommandsFor t (pat,contentMatch) = do mvar <- newEmptyMVar
                                                             startServer terrain (fromIntegral serverPort)  mvar          
                                                             output <- simpleHttpContent ("http://127.0.0.1:" ++ show serverPort ++ pat)
                                                             simpleHttpContent ("http://127.0.0.1:" ++ show serverPort ++ "/Exit")
                                                             output @?= contentMatch
                                                             putMVar mvar () 
  
interactThroughAnHttpServer = 
  "interacting with Crete 1941 through a REST API"  `shouldBe`
  matchReplyContentToRESTCommandsFor terrain `with`
  [
    ("/GetUnitLocations", "\"unit\": \"Campbell\", zone: \"Rethymnon\"")
  ]
  
