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

matchReplyContentToRESTCommandsFor t (pat,contentMatch) = do output <- simpleHttpContent ("http://127.0.0.1:" ++ show serverPort ++ pat)
                                                             simpleHttpContent ("http://127.0.0.1:" ++ show serverPort ++ "/Exit")
                                                             output @?= contentMatch
  
interactThroughAnHttpServer = 
  "interacting with Crete 1941 through a REST API"  `shouldBe`
  matchReplyContentToRESTCommandsFor terrain `with`
  [
    ("/GetUnitLocations", "{\"UnitLocations\":[[\"Campbell\",\"Rethymnon\"],[\"arm1\",\"Rethymnon\"],[\"hq1\",\"Beach\"],[\"inf1\",\"Rethymnon\"]]}"),
    ("/GetUnitStatus",   "{\"UnitStatus\":[[\"Campbell\",{\"unitName\":\"Campbell\",\"unitSide\":\"British\",\"unitState\":\"Full\",\"unitStrength\":{\"offense\":0,\"defense\":3,\"movement\":8},\"unitType\":\"DivisionHQ\",\"unitHq\":\"Nothing\"}],[\"arm1\",{\"unitName\":\"arm1\",\"unitSide\":\"German\",\"unitState\":\"Full\",\"unitStrength\":{\"offense\":0,\"defense\":3,\"movement\":1},\"unitType\":\"Armoured\",\"unitHq\":\"Nothing\"}],[\"hq1\",{\"unitName\":\"hq1\",\"unitSide\":\"German\",\"unitState\":\"Full\",\"unitStrength\":{\"offense\":0,\"defense\":3,\"movement\":8},\"unitType\":\"DivisionHQ\",\"unitHq\":\"Nothing\"}],[\"inf1\",{\"unitName\":\"Campbell\",\"unitSide\":\"British\",\"unitState\":\"Full\",\"unitStrength\":{\"offense\":0,\"defense\":3,\"movement\":0},\"unitType\":\"Infantry\",\"unitHq\":{\"Just\":{\"unitName\":\"Campbell\",\"unitSide\":\"British\",\"unitState\":\"Full\",\"unitStrength\":{\"offense\":0,\"defense\":3,\"movement\":8},\"unitType\":\"DivisionHQ\",\"unitHq\":\"Nothing\"}}}]]}")
  ]
  
