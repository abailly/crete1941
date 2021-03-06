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
import Network.HTTP.Types(statusBadRequest)
import Network.URI
import Data.Maybe      
import Control.Concurrent(killThread,newEmptyMVar,putMVar)

-- From http://book.realworldhaskell.org/read/extended-example-web-client-programming.html      
simpleHttpContent verb url =
    do resp <- httpRequest verb url
       case resp of
         Left x -> return $ ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ (show r)
                   Just url -> simpleHttpContent verb url
               _ -> return $ (show r)
    
httpRequest verb url = let uri = fromJust $ parseURI url
                       in simpleHTTP $ Request {rqURI = uri, rqMethod = verb,rqHeaders = [],rqBody = ""}

matchReplyContentToRESTCommandsFor t ((verb,pat),contentMatch) = do output <- simpleHttpContent verb ("http://127.0.0.1:" ++ show serverPort ++ pat)
                                                                    simpleHttpContent POST ("http://127.0.0.1:" ++ show serverPort ++ "/exit")
                                                                    output ~?~ contentMatch
  
responseStatusOf verb uri = httpRequest verb ("http://127.0.0.1:" ++ show serverPort ++ uri) >>= either (\e -> error $ "unexpected result" ++ show e) (return.rspCode) 
responseTypeOf verb uri = httpRequest verb ("http://127.0.0.1:" ++ show serverPort ++ uri) >>= 
                     either (\e -> error $ "unexpected result" ++ show e) (return.fromJust.findHeader HdrContentType) 
                          
interactThroughAnHttpServer =  TestList [
  "interacting with Crete 1941 through a REST API"  `shouldBe`
  matchReplyContentToRESTCommandsFor terrain `with`
  [
    ((GET, "/units/locations"), "\\{\"UnitLocations\".*\\}"),
    ((GET, "/units/status"),   "\\{\"UnitStatus\":.*\\}"),
    ((GET, "/unit/arm1"),   "\\{\"UnitStatus\":.*\\}"),
    ((POST, "/unit/arm1/move?to=Beach"),   "\\{\"UnitMoved\":.*\\}"),
    ((POST,"/unit/arm1/attack?target=inf1"),   "\\{\"UnitReduced\":.*\\}"),
    -- error cases
    ((POST,"/unit/arm1/move?to=foo"),   "\\{\"ErrorInCommands\":\"zone foo is not a valid zone name\"\\}"),
    ((POST,"/unit/unknown/move?to=Beach"),   "\\{\"ErrorInCommands\":\"unit unknown is not a valid unit name\"\\}")
  ],
  "serving static resources"  `should` [ 
    "respond '404 NOT FOUND' for non existing resource" `for`  
    responseStatusOf GET "/does/not/exist" >>= assertEqual "incorrect response status" (4,0,4),
    
    "respond '200 OK' for existing resource" `for`  
    responseStatusOf GET "/resources/images/1-7th-rtr.png" >>= assertEqual "incorrect response status" (2,0,0),
    
    "provide correct mime-type for existing resource" `for`  
    responseTypeOf GET "/resources/images/carte1.jpg" >>= assertEqual "incorrect response status" "image/jpeg"
    ],
  "routing actions" `should` [
    "reject request if method is incorrect" `for`
    responseStatusOf GET "/unit/arm1/move?to=Beach" >>= assertEqual "incorrect response status" (4,0,4)
    ]
  ]
  
