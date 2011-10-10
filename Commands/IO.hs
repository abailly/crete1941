{-# LANGUAGE PatternGuards, MultiParamTypeClasses #-}
module Commands.IO where
import CommandsInterpreter
import Text.ParserCombinators.ReadP
import Data.Char(isSpace)
import Control.Monad.Error
import System.Exit(exitWith,ExitCode(..))

readCommand = do input <- getLine 
                 case decode input of 
                   Right c -> return c
                   Left  m -> return $ CommandError m
writeResult (Msg str) = putStrLn $ (unlines str)
writeResult r         = putStrLn $ show r
writeMessage m        = putStrLn $ m
doExit                = exitWith ExitSuccess

commandsMap = [("getunitlocations",GetUnitLocations),
               ("getunitstatus",GetUnitStatus)
              ]

decode :: String -> Either String Command
decode s | [x] <- parse = Right x 
         | otherwise    = Left s
  where parse = [x | (x,t) <- (readP_to_S parseCommand s) ]

parseCommand = do (getUnitLocationsParser +++ 
                   getUnitStatusParser    +++ 
                   moveCommandParser      +++
                   helpParser             +++
                   exitParser)
            where
              helpParser             = string "help" +++ string "Help" >> return Help
              exitParser             = string "exit" +++ string "Exit" >> return Exit
              getUnitLocationsParser = string "GetUnitLocations" +++ string "getunitlocations" >> return GetUnitLocations
              getUnitStatusParser    = string "getunitStatus" +++ string "GetUnitStatus" >> return GetUnitStatus
              moveCommandParser   = do string "move" ; skipSpaces
                                       uname <- munch (not . isSpace) ; skipSpaces
                                       zname <- munch (not . isSpace) ; skipSpaces
                                       return $ MoveUnit uname zname

