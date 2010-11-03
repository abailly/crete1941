module Commands.JSON where
import Control.Arrow(second)

import Text.JSON(toJSObject,JSValue(..),showJSON,JSON,toJSString,encode)

import Common
import Units
import MovementRules
import CommandsInterpreter

instance JSON Side where
  showJSON German  = JSString $ toJSString "German"
  showJSON British = JSString $ toJSString "British"
  
instance JSON UnitState where
  showJSON (UnitState offense defense movement) = JSObject $ toJSObject [("offense",showJSON offense),("defense", showJSON defense),("movement",showJSON movement)]
  
instance JSON MovementRules.State where
  showJSON Full         = showJSON "Full"
  showJSON Reduced      = showJSON "Reduced"
  showJSON Disorganized = showJSON "Disorganized"
  
instance JSON UnitType where
  showJSON Artillery  = showJSON "Artillery "
  showJSON Armoured   = showJSON "Armoured"
  showJSON Infantry   = showJSON "Infantry"
  showJSON Flak       = showJSON "Flak"
  showJSON DivisionHQ = showJSON "DivisionHQ"
  showJSON RegimentHQ = showJSON "RegimentHQ"
               
instance JSON Unit where
  showJSON (Unit unitNam unitSide unitState unitStrength unitType unitHq) = JSObject $ toJSObject [("unitName", showJSON unitNam),
                                                                                                    ("unitSide", showJSON unitSide),
                                                                                                    ("unitState", showJSON unitState),
                                                                                                    ("unitStrength", showJSON unitStrength),
                                                                                                    ("unitType", showJSON unitType),
                                                                                                    ("unitHq", showJSON (fmap unitName unitHq))]
  
instance JSON CommandResult where
  showJSON (UnitStatus units)   = JSObject $ toJSObject [("unitStatus", JSObject $ toJSObject (map (second $ showJSON) units))]
  showJSON (UnitLocations locs) = JSObject $ toJSObject [("unitLocations", JSObject $ toJSObject (map (second $ JSString . toJSString) locs))]
  showJSON Bye                  = JSObject $ toJSObject [("exit",JSBool True)]
