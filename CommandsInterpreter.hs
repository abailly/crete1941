{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveDataTypeable, FunctionalDependencies, TupleSections #-}
module CommandsInterpreter where
import MovementRules
import CombatRules
import Terrain
import Control.Monad
import Control.Monad.State
import Debug.Trace
import qualified Data.Generics as G

data Command = GetUnitLocations 
             | GetUnitStatus
             | SingleUnitStatus Name
             | MoveUnit Name Name
             | Attack Name Name
             | CommandError String
             | Help
             | Exit 
              deriving (Eq, Show, Read)
                       
data CommandResult = UnitLocations [(Name,Name)]
                   | UnitStatus [(Name,Unit)]
                   | UnitMoved Name Name
                   | UnitReduced Name
                   | UnitEliminated Name
                   | MoveProhibited Name Name
                   | CombatResult [CommandResult]
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
executeCommand (Attack an dn)   = do t <- get
                                     let (m,t') = (runState . runBattle) (combat an dn) t
                                     put t'
                                     return $ CombatResult (map (combatResult t') m)

combatResult t (Reduce u)    = UnitReduced (unitName u)
combatResult t (Eliminate u) = UnitEliminated (unitName u)
combatResult t (Retreat u)   = UnitMoved un (unitLocation un t)
  where un = unitName u
                                     
displayAbout = ["Crete 1941, A Wargame Simulating German Air Assault on Crete",
                "Game Design by Vae Victis (1998), coded by Arnaud Bailly (2010)"]
               
displayHelp = ["Available commands:",
               "GetUnitLocations: gives the zone name where each unit is located",
               "GetUnitStatus   : provides the status of each unit (current location, current strength, characteristics...)",
               "Exit            : get outta here",
               "Help            : display this help",
               "Move <unit> <to>: moves the given unit identified by its name to the given zone"]

