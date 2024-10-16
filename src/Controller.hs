{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
    ( Movable(glide),
      GameState(..),
      initialState,
      wPressed,
      aPressed,
      dPressed,
      Status (GameOver, PreStart, Running, Paused, FirstStep),
      CanShoot (shootBullet),
      IsRound (pColliding, checkWithinBounds) )
import Functionality
    ( pCheckBounds,
      sbColliding,
      randomSteen,
      checkMovementKeyPressed )
import Constants ( steenScoreMultiplier, highscorePath )
import General ( addMaybe )
import Graphics.Gloss.Interface.IO.Game
    ( Key(SpecialKey, Char),
      KeyState(Up, Down),
      SpecialKey(KeyEsc, KeyEnter),
      Event(EventKey) )
import System.Random ( randomIO, Random (random) )
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Foreign (new)






step :: Float -> GameState -> IO GameState
step _ gstate 
    | status gstate == FirstStep = readHighscore gstate
    | status gstate == GameOver || status gstate == Paused || status gstate == PreStart = return gstate 
    | any (pColliding (player gstate)) (stenen gstate) ||
      any (pColliding (player gstate)) (alienBullets gstate)  
        = finishGame gstate            
    | otherwise = do r <- randomIO
                     return (update gstate r)

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
           player = pCheckBounds (glide (foldr checkMovementKeyPressed (player gstate) movementKeys))
         , stenen = addMaybe (randomSteen r gstate) (map glide (filter checkWithinBounds notShotDownStenen))
         , bullets = map glide (filter checkWithinBounds (bullets gstate))
         , alienBullets = alienBullets gstate
         , score = score gstate + steenScoreMultiplier * (length (stenen gstate) - length notShotDownStenen) 
         }
  where  
    notShotDownStenen = filter (\steen -> not (any (sbColliding steen) (bullets gstate))) (stenen gstate)
    movementKeys = [('w', wPressed gstate), ('a', aPressed gstate), ('d', dPressed gstate)] 
    





-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate) 

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'r') Down _ _) GameState { status = GameOver} = initialState
inputKey k@(EventKey (Char 'w') Down _ _) gstate@(GameState { status = PreStart }) -- if w is pressed for the first time, start the game and call inputkey again to move forward
    = inputKey k (gstate { status = Running })

inputKey (EventKey (SpecialKey KeyEnter) Down _ _) gstate@(GameState { status = Running }) 
    = shootBullet (player gstate) gstate

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

