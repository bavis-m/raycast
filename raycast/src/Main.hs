import Foreign
import Foreign.C.Types
import Data.IORef
import Engine.RuntimeInterface
import Game.Game
import Game.Level
import Data.Vect.Double.Base
import System.Environment
import Control.Monad.Trans.Except

main = do
  ok <- initRuntime
  
  if ok then do
    args <- getArgs
    
    levelText <- readFile $ "resources/levels/" ++ (args !! 0)
    
    tryGameState <- runExceptT $ parseAndLoad levelText
    case tryGameState of
      Left err -> putStrLn err
      Right initialGameState ->
        do    
          runEngine initialGameState updateState          
          return ()
    
  else return()
