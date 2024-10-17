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






step :: Float -> GameState -> IO GameState
step _ gstate
    | status gstate == FirstStep = readHighscore gstate
    | status gstate == GameOver || status gstate == Paused || status gstate == PreStart = return gstate
    | any (pColliding p) (stenen gstate) ||
      any (pColliding p) (aliens gstate) ||
      any (pColliding p) (alienBullets gstate)
        = finishGame gstate
    | otherwise = update gstate <$> randomIO
  where p = player gstate

readHighscore :: GameState -> IO GameState
readHighscore gstate
    = do text <- readFile highscorePath
         let oldHs = readMaybe (takeWhile (/= '\n') text)
         return $ gstate { status = PreStart, highscore = fromMaybe 0 oldHs }

finishGame :: GameState -> IO GameState
finishGame gstate
    = do when (score gstate > highscore gstate)
           $ writeFile highscorePath (show (score gstate))
         return $ gstate { status = GameOver}

update :: GameState -> Int -> GameState
update gstate r
    =  gstate
         {
           player = pCheckBounds (glide (foldr checkMovementKeyPressed (player gstate) keysPressed))
         , stenen = stenenUpdated
         , bullets = updateLocations (bullets gstate)
         , aliens = aliensUpdated
         , alienBullets = addMaybe (newBullet gstate r) (updateLocations (alienBullets gstate))
         , score = score gstate + steenScoreMultiplier * stenenShot + alienScoreMultiplier * aliensShot
         }
  where
    keysPressed = [('w', wPressed gstate), ('a', aPressed gstate), ('d', dPressed gstate)]

    (stenenUpdated, stenenShot) = updateRemoveAndAdd gstate r (stenen gstate)
    (aliensUpdated, aliensShot) = updateRemoveAndAdd gstate r (aliens gstate)






-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'r') Down _ _) gstate@(GameState { status = GameOver }) 
    = initialState (ufoPic gstate)

inputKey k@(EventKey (Char 'w') Down _ _) gstate@(GameState { status = PreStart }) -- if w is pressed for the first time, start the game and call inputkey again to move forward
    = inputKey k (gstate { status = Running })

inputKey (EventKey (SpecialKey KeyEnter) Down _ _) gstate@(GameState { status = Running })
    = gstate { bullets = bul : bullets gstate, score = score gstate - 1 }
  where bul = shootBullet (player gstate) gstate

inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState { status = Running })
    = gstate { status = Paused }
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState { status = Paused })
    = gstate { status = Running }

inputKey (EventKey (Char 'w') Down _ _) gstate = gstate { wPressed = True }
inputKey (EventKey (Char 'w') Up _ _) gstate = gstate { wPressed = False }
inputKey (EventKey (Char 'a') Down _ _) gstate = gstate { aPressed = True }
inputKey (EventKey (Char 'a') Up _ _) gstate = gstate { aPressed = False }
inputKey (EventKey (Char 'd') Down _ _) gstate = gstate { dPressed = True }
inputKey (EventKey (Char 'd') Up _ _) gstate = gstate {dPressed = False }

-- inputKey (EventKey k Down _ _) gstate = gstate { keysPressed = insert k (keysPressed gstate)} -- for other keys
-- inputKey (EventKey k Up _ _)   gstate = gstate { keysPressed = delete k (keysPressed gstate)}
inputKey _ gstate = gstate -- other key events (and events in general)

