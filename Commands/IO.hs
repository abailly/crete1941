module Commands.IO where
import CommandsInterpreter
import Text.ParserCombinators.ReadP
import Data.Char(isSpace)

commandsMap = [("getunitlocations",GetUnitLocations),
               ("getunitstatus",GetUnitStatus)
              ]

decode :: String -> Command
decode s = x where (x,_):_ = (readP_to_S parseCommand s)

parseCommand = do (getUnitLocationsParser +++ 
                   getUnitStatusParser    +++ 
                   moveCommandParser)
            where
              getUnitLocationsParser = string "GetUnitLocations" +++ string "getunitlocations" >> return GetUnitLocations
              getUnitStatusParser    = string "getunitStatus" +++ string "GetUnitStatus" >> return GetUnitStatus
              moveCommandParser   = do string "move" ; skipSpaces
                                       uname <- munch (not . isSpace) ; skipSpaces
                                       zname <- munch (not . isSpace) ; skipSpaces
                                       return $ MoveUnit uname zname

