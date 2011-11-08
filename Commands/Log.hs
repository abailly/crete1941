-- |Handle notifications for Server events.
module Commands.Log(module Yesod.Logger, 
                    Loggable(..),
                    notifyServerEvent,
                    ServerEvent(..)) where

import Yesod.Logger(logString, Logger,makeLogger,flushLogger)
import Network.Wai(Request(..), Response(..))
import Network.HTTP.Types(statusCode)
import Data.Time(getCurrentTime, diffUTCTime, UTCTime, formatTime)
import System.Locale(defaultTimeLocale)
import Text.Printf(printf)

class Loggable l where
  getLogger     :: l -> Logger
  getIdentifier :: l -> String
  getStartTime  :: l -> UTCTime
  
data ServerEvent = ServerStart 
                 | ServerStop
                 | HandleRequest Request Response UTCTime
                   
notifyServerEvent :: (Loggable l) =>  
                     l -> ServerEvent -> IO l
notifyServerEvent s ServerStart =  getCurrentTime >>= 
                                   (\start -> logString (getLogger s) $ printf "[%s] (0.0s) %s %0d"
                                             (formatTime defaultTimeLocale "%Y%m%d%H%M%S%Q" start) 
                                             "starting server " (getIdentifier s))
                                   >> return s

notifyServerEvent s ServerStop =  getCurrentTime >>= 
                                  (\time -> logString (getLogger s) $ printf "[%s] (%s) %s %0d"
                                            (formatTime defaultTimeLocale "%Y%m%d%H%M%S%Q" time) 
                                            (show $ diffUTCTime time (getStartTime s))
                                            "stopping server " (getIdentifier s))
                                  >> return s
notifyServerEvent s (HandleRequest req (ResponseBuilder st _ _) start) = getCurrentTime >>=
                                                    (\end -> logString (getLogger s) $ printf "[%s] (%s) %s %s %s %s" 
                                                             (formatTime defaultTimeLocale "%Y%m%d%H%M%S%Q" end) 
                                                             (show $ diffUTCTime end start)
                                                             (getIdentifier s) 
                                                             (show $ remoteHost req) 
                                                             (show $ requestMethod req) 
                                                             (show $ rawPathInfo req) 
                                                             (show $ statusCode st))
                                                    >> return s
  
