{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module CommandsInterpreter where
import MovementRules
import Terrain
import Control.Monad
import Control.Monad.State
import Debug.Trace


data Command = GetUnitLocations 
             | GetUnitStatus
             | MoveUnit Name Name
              deriving (Eq, Show, Read)
                       
data CommandResult = UnitLocations [(Name,Name)]
                   | UnitStatus [(Name,Unit)]
                   | UnitMoved Name Name
                   | MoveProhibited Name Name
              deriving (Eq, Show, Read)

-- |Low-level I/O routines for command interaction
-- Abstract away from the details of 
class (Monad io) => CommandIO io where
  readCommand :: io Command
  writeResult :: CommandResult -> io ()
  
-- |Data type for handling commands execution within a certain context.
-- This type is parameterized by the inner monad used for handling low-level I/O. This monad
-- should actually be a CommandIO monad, but this prevents making Commands an instance of
-- MonadTrans as lift requires only a Monad and we cannot constraint more the context than
-- what is required by the interface
newtype (Monad io,BattleMap t) => Commands t io a = Commands { runCommands :: StateT t io a }
    deriving (Monad, MonadState t)

instance (BattleMap t) => MonadTrans (Commands t) where 
  lift = liftCommands
  
liftCommands :: (BattleMap t, Monad m) => m a -> Commands t m a
liftCommands m  = Commands $ StateT (\ st ->                -- This starts the function for state transformation
                                      (m >>=                -- here we are in the inner monad context, so we sequence the computation with ... 
                                       \ x -> return (x,st) -- a computation that packages in the inner monad the result of computation with state
                                      )
                                    )

interpret ::  (CommandIO io,BattleMap t) => Commands t io CommandResult
interpret = do c <- lift readCommand
               r <- executeCommand c
               lift $ writeResult r
               return r
               
executeCommand ::  (CommandIO io,BattleMap t) => Command -> Commands t io CommandResult
executeCommand GetUnitLocations = get >>= return . UnitLocations . allUnitLocations
executeCommand GetUnitStatus    = get >>= return . UnitStatus . allUnitStatus
executeCommand (MoveUnit un zn) = do t <- get
                                     let m = (evalState . runBattle) (prepareMove un zn) t
                                     return $ if (zoneName . toZone) m == zn then
                                                UnitMoved un zn
                                              else
                                                MoveProhibited un zn
                                                
