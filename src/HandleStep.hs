module HandleStep where

import DataTypes
import MorePlayerLogic
import Constants
import System.Random
import Data.Maybe
import Control.Monad
import CanGetHitByPlayerBulletClass
import Text.Read (readMaybe)
import TempObjectClass
import MovableClass
import HasAnimationClass
import RandomObjectClass
import Data.Foldable






step :: Float -> GameState -> IO GameState -- gets called every frame
step _ menu@(Menu {}) -- if the menu is open, do nothing
    = return menu 
step secs gstate      -- if the actual game is open: 
    | status gstate == FirstStep -- if this is the first step of the game
        = do r <- newStdGen -- create random generator
             firstStep gstate r
    | status gstate == Running && aPlayerHitsSomething gstate -- if a player hits something (alien, steen or bullet)
        = finishGame gstate
    | otherwise 
        = return $ pureStep secs gstate


firstStep :: GameState -> StdGen -> IO GameState -- this function reads the highscore, and sets the gen of the gamestate to the provided gen (to create random numbers)
firstStep gstate r
    = do text <- readFile highscorePath
         let scores = lines text
         return $ gstate { status = PreStart,
                           gen = r,
                           highscore = maybe 0 read (maybeLast scores) }
  where maybeLast = foldl' (\_ x -> Just x) Nothing -- last line of the file

finishGame :: GameState -> IO GameState -- this function stops the game and writes the score (if it is higher than the highscore) to the highscore file
finishGame gstate
    = do when (score gstate > highscore gstate)
             $ appendFile highscorePath (show (score gstate) ++ "\n")
         return $ gstate { status = GameOver}



pureStep :: Float -> GameState -> GameState
pureStep secs gstate
    | status gstate /= Running -- if the game is not running, do nothing
        = gstate
    | elapsedTime gstate + secs > 1 / gameTicksPerSec -- if the game is running and it is time for a game tick
        = updateEveryFrame secs (updateEveryTick (gstate { elapsedTime = elapsedTime gstate + secs - 1 / gameTicksPerSec}))
             -- ^ we choose this exact elapsedTime instead of 0 so that lower framerates don't unnecessarily put elapsedTime at 0 all the time (which lowers the tick rate)
    | otherwise -- if the game is running and it's not time for a tick
        = updateEveryFrame secs (gstate { elapsedTime = elapsedTime gstate + secs })

updateEveryFrame :: Float -> GameState -> GameState -- gets called every frame
updateEveryFrame secs gstate
    = gstate
        {
          player = updatePlayer (player gstate)                   
        , player2 = updatePlayer <$> player2 gstate              
        , stenen = updateLocations secs remainingStenen              -- updates the locations of the stenen
        , bullets = updateLocations secs (bullets gstate)            -- updates the locations of the player bullets
        , aliens = updateLocations secs remainingAliens              -- updates the locations of the aliens
        , alienBullets = updateLocations secs (alienBullets gstate)  -- updates the locations of the alien bullets
        , score = score gstate + steenScoreMultiplier * nrStenenShot + alienScoreMultiplier * nrAliensShot -- updates the score
        }
  where
    (remainingStenen, nrStenenShot) = checkBulletHitsAndUpdateAnims secs gstate (stenen gstate) -- gives the number of stenen shot this frame, updates the implosion animations (if necessary), and filters out the stenen whose implosion animation has ended
    (remainingAliens, nrAliensShot) = checkBulletHitsAndUpdateAnims secs gstate (aliens gstate) -- same as for stenen
    updatePlayer = updateAnim secs . pCheckBounds . updateLocation secs                         -- updates the player location, handles collision with the walls, and updates the boost animation (if necessary)

updateEveryTick :: GameState -> GameState -- gets called every game tick
updateEveryTick gstate
    =  gstate
         {
           player = updatePlayer (player gstate)                           
         , player2 = updatePlayer <$> player2 gstate                        
         , stenen = addMaybe perhapsNewSteen (stenen gstate)                   
         , aliens = addMaybe perhapsNewAlien (aliens gstate)
         , alienBullets = addMaybe perhapsNewAlienBullet (alienBullets gstate)
         , gen = newGen -- update the gen so that we keep producing pseudorandom values
         }
  where
    (perhapsNewSteen, gen1) = perhapsCreateNew gstate (gen gstate) -- might create a new steen (small chance, most of the time perhapsNewSteen is Nothing) 
    (perhapsNewAlien, gen2) = perhapsCreateNew gstate gen1         -- might create a new alien (small chance)
    (perhapsNewAlienBullet, newGen) = perhapsCreateNew gstate gen2 -- might create a new alien bullet (small chance)

    updatePlayer = autoDecceleration . checkMovementKeysPressed    -- applies the automatic decceleration, and applies boosting/steering if one of those keys is currently pressed

    addMaybe mx xs = maybe xs (: xs) mx