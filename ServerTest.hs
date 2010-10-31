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
import Control.Concurrent(killThread,newEmptyMVar)
-- From http://book.realworldhaskell.org/read/extended-example-web-client-programming.html      
simpleHttpContent url =
    do resp <- simpleHTTP request
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
                                                             tid <- startServer t (fromIntegral serverPort) mvar
                                                             output <- simpleHttpContent ("http://127.0.0.1:" ++ show serverPort ++ pat)
                                                             output @?= contentMatch
                                                             killThread tid
                                                             
  
interactThroughAnHttpServer = 
  "interacting with Crete 1941 through a REST API"  `shouldBe`
  matchReplyContentToRESTCommandsFor terrain `with`
  [
    ("/GetUnitLocations", "\"unit\": \"Campbell\", zone: \"Rethymnon\"")
  ]
  
