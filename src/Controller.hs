module Controller where

import Model
import Functionality
import Constants
import General
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe
import Control.Monad
import Text.Read (readMaybe)
import Data.List (find)
import Graphics.Gloss.Data.Vector (mulSV)






step :: Float -> GameState -> IO GameState
step _ menu@(Menu {}) = return menu 
step secs gstate
    | status gstate == FirstStep = readHighscore gstate
    | status gstate == GameOver || status gstate == Paused || status gstate == PreStart = return gstate
    | aPlayerHitsSomething gstate = finishGame gstate
    | elapsedTime gstate + secs > 1 / bigUpdatesPerSec 
        = do r <- randomIO
             return $ updateEveryStep secs (updatePerTimeUnit r (gstate { elapsedTime = elapsedTime gstate + secs - 1 / bigUpdatesPerSec}))
             -- ^ we choose this exact elapsedTime instead of 0 so that lower framerates don't unnecessarily put elapsedTime at 0 all the time (which makes everything slower)
    | otherwise 
        = return $ updateEveryStep secs (gstate { elapsedTime = elapsedTime gstate + secs })


readHighscore :: GameState -> IO GameState
readHighscore gstate
    = do text <- readFile highscorePath
         let scores = lines text
         return $ gstate { status = PreStart, 
                           highscore = case length scores of
                                       0 -> 0
                                       _ -> fromMaybe 0 (readMaybe (last scores)) } 

finishGame :: GameState -> IO GameState
finishGame gstate
    = do when (score gstate > highscore gstate)   
             $ appendFile highscorePath (show (score gstate) ++ "\n") 
         return $ gstate { status = GameOver}

updateEveryStep :: Float -> GameState -> GameState
updateEveryStep secs gstate 
    = gstate 
        {
          player = updatePlayer (player gstate)
        , player2 = player2 gstate >>= Just . updatePlayer
        , stenen = updateLocations secs remainingStenen
        , bullets = updateLocations secs (bullets gstate)
        , aliens = updateLocations secs remainingAliens
        , alienBullets = updateLocations secs (alienBullets gstate)
        , score = score gstate + steenScoreMultiplier * nrStenenShot + alienScoreMultiplier * nrAliensShot 
        }
  where 
    (remainingStenen, nrStenenShot) = checkBulletHitsAndUpdateAnims secs gstate (stenen gstate) 
    (remainingAliens, nrAliensShot) = checkBulletHitsAndUpdateAnims secs gstate (aliens gstate)
    updatePlayer = updateAnim secs . pCheckBounds . glide secs

updatePerTimeUnit :: Int -> GameState -> GameState
updatePerTimeUnit r gstate
    =  gstate
         {
           player = updatePlayer (player gstate)
         , player2 = player2 gstate >>= Just . updatePlayer
         , stenen = addMaybe (perhapsCreateNew gstate r) (stenen gstate)
         , aliens = addMaybe (perhapsCreateNew gstate r) (aliens gstate)
         , alienBullets = addMaybe (newBullet gstate r) (alienBullets gstate)
         }
  where
    keysPressed p = [('w', forwardPressed p), ('a', leftPressed p), ('d', rightPressed p)]
    updatePlayer = pAutoDecceleration . checkMovementKeysPressed






-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)


inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'm') Down _ _) gstate@(GameState { status = GameOver }) 
    = Menu (ufoPic gstate) (steenAnimPics gstate) (ufoAnimPics gstate) (boostAnimPics gstate)

inputKey k@(EventKey (SpecialKey KeySpace) Down _ _) gstate@(GameState { status = PreStart }) -- if w is pressed for the first time, start the game and call inputkey again to move forward
    = inputKey k (gstate { status = Running })

inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate@(GameState { status = Running })
    = playerShoots (player gstate) gstate
inputKey (EventKey (Char 'v') Down _ _) gstate@(GameState { status = Running, player2 = Just p })
    = playerShoots p gstate

inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState { status = Running })
    = gstate { status = Paused }
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState { status = Paused })
    = gstate { status = Running }

inputKey (EventKey (SpecialKey KeyUp) Down _ _)    gstate@(GameState {}) = gstate { player = (player gstate) { forwardPressed = True } }
inputKey (EventKey (SpecialKey KeyUp) Up _ _)      gstate@(GameState {}) = gstate { player = (player gstate) { forwardPressed = False } }
inputKey (EventKey (SpecialKey KeyLeft) Down _ _)  gstate@(GameState {}) = gstate { player = (player gstate) { leftPressed = True } }
inputKey (EventKey (SpecialKey KeyLeft) Up _ _)    gstate@(GameState {}) = gstate { player = (player gstate) { leftPressed = False } }
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate@(GameState {}) = gstate { player = (player gstate) { rightPressed = True } }
inputKey (EventKey (SpecialKey KeyRight) Up _ _)   gstate@(GameState {}) = gstate { player = (player gstate) { rightPressed = False } }

inputKey (EventKey (Char 'w') Down _ _)  gstate@(GameState { player2 = Just p2}) = gstate { player2 = Just p2 { forwardPressed = True } }
inputKey (EventKey (Char 'w') Up _ _)    gstate@(GameState { player2 = Just p2}) = gstate { player2 = Just p2 { forwardPressed = False } }
inputKey (EventKey (Char 'a') Down _ _)  gstate@(GameState { player2 = Just p2}) = gstate { player2 = Just p2 { leftPressed = True } }
inputKey (EventKey (Char 'a') Up _ _)    gstate@(GameState { player2 = Just p2}) = gstate { player2 = Just p2 { leftPressed = False } }
inputKey (EventKey (Char 'd') Down _ _)  gstate@(GameState { player2 = Just p2}) = gstate { player2 = Just p2 { rightPressed = True } }
inputKey (EventKey (Char 'd') Up _ _)    gstate@(GameState { player2 = Just p2}) = gstate { player2 = Just p2 { rightPressed = False } }

inputKey (EventKey (MouseButton LeftButton) Down _ p) menu@(Menu {}) = newState
  where
    but = find (withinButtonBounds p) [singleButton, multiButton]

    newState | but == Just singleButton = initState
             | but == Just multiButton  = multState
             | otherwise                = menu

    initState = initialState (ufoPic menu) (steenAnimPics menu) (ufoAnimPics menu) (boostAnimPics menu)
    multState = initState { player2 = Just initialPlayer2 }

inputKey _ gstate = gstate -- other key events (and events in general)





