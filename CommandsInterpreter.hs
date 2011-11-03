{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveDataTypeable, FunctionalDependencies, TupleSections #-}
module CommandsInterpreter where
import MovementRules
import Terrain
import Control.Monad
import Control.Monad.State
import Debug.Trace
import qualified Data.Generics as G

data Command = GetUnitLocations 
             | GetUnitStatus
             | SingleUnitStatus Name
             | MoveUnit Name Name
             | CommandError String
             | Help
             | Exit 
              deriving (Eq, Show, Read)
                       
data CommandResult = UnitLocations [(Name,Name)]
                   | UnitStatus [(Name,Unit)]
                   | UnitMoved Name Name
                   | MoveProhibited Name Name
                   | ErrorInCommands String
                   | Msg [String]
                   | Bye
              deriving (Eq, Show, Read, G.Data, G.Typeable)

executeCommand ::  (BattleMap t, MonadState t s) => Command -> s CommandResult
executeCommand GetUnitLocations = get >>= return . UnitLocations . allUnitLocations
executeCommand GetUnitStatus    = get >>= return . UnitStatus . allUnitStatus
executeCommand (SingleUnitStatus un) = get >>= return . UnitStatus . (:[]) . (un,). unit un
executeCommand (CommandError s) = return $ ErrorInCommands ("unknown command: " ++ s)
executeCommand Exit             = return Bye
executeCommand Help             = return $ Msg (displayAbout ++ displayHelp)
executeCommand (MoveUnit un zn) = do t <- get
                                     let (m,t') = (runState . runBattle) (move un zn) t
                                     put t'
                                     return $ 
                                       case m of
                                         NoMove -> MoveProhibited un zn
                                         _      -> UnitMoved un zn

displayAbout = ["Crete 1941, A Wargame Simulating German Air Assault on Crete",
                "Game Design by Vae Victis (1998), coded by Arnaud Bailly (2010)"]
               
displayHelp = ["Available commands:",
               "GetUnitLocations: gives the zone name where each unit is located",
               "GetUnitStatus   : provides the status of each unit (current location, current strength, characteristics...)",
               "Exit            : get outta here",
               "Help            : display this help",
               "Move <unit> <to>: moves the given unit identified by its name to the given zone"]

