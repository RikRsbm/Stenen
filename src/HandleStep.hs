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
import Data.Foldable (Foldable(foldl'))






step :: Float -> GameState -> IO GameState
step _ menu@(Menu {}) = return menu
step secs gstate
    | status gstate == FirstStep
        = do r <- newStdGen
             firstStep gstate r
    | status gstate == Running && aPlayerHitsSomething gstate
        = finishGame gstate
    | otherwise
        = return $ pureStep secs gstate


firstStep :: GameState -> StdGen -> IO GameState
firstStep gstate r
    = do text <- readFile highscorePath
         let scores = lines text
         return $ gstate { status = PreStart,
                           gen = r,
                           highscore = maybe 0 read (maybeLast scores) }
  where maybeLast = foldl' (\_ x -> Just x) Nothing

finishGame :: GameState -> IO GameState
finishGame gstate
    = do when (score gstate > highscore gstate)
             $ appendFile highscorePath (show (score gstate) ++ "\n")
         return $ gstate { status = GameOver}



pureStep :: Float -> GameState -> GameState
pureStep secs gstate
    | status gstate == GameOver || status gstate == Paused || status gstate == PreStart
        = gstate
    | elapsedTime gstate + secs > 1 / bigUpdatesPerSec
        = updateEveryStep secs (updatePerTimeUnit (gstate { elapsedTime = elapsedTime gstate + secs - 1 / bigUpdatesPerSec}))
             -- ^ we choose this exact elapsedTime instead of 0 so that lower framerates don't unnecessarily put elapsedTime at 0 all the time (which makes everything slower)
    | otherwise
        = updateEveryStep secs (gstate { elapsedTime = elapsedTime gstate + secs })

updateEveryStep :: Float -> GameState -> GameState
updateEveryStep secs gstate
    = gstate
        {
          player = updatePlayer (player gstate)
        , player2 = updatePlayer <$> player2 gstate
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

updatePerTimeUnit :: GameState -> GameState
updatePerTimeUnit gstate
    =  gstate
         {
           player = updatePlayer (player gstate)
         , player2 = updatePlayer <$> player2 gstate
         , stenen = addMaybe perhapsNewSteen (stenen gstate)
         , aliens = addMaybe perhapsNewAlien (aliens gstate)
         , alienBullets = addMaybe perhapsNewAlienBullet (alienBullets gstate)
         , gen = newGen
         }
  where
    (perhapsNewSteen, gen1) = perhapsCreateNew gstate (gen gstate)
    (perhapsNewAlien, gen2) = perhapsCreateNew gstate gen1
    (perhapsNewAlienBullet, newGen) = perhapsCreateNew gstate gen2

    keysPressed p = [('w', forwardPressed p), ('a', leftPressed p), ('d', rightPressed p)]
    updatePlayer = autoDecceleration . checkMovementKeysPressed

    addMaybe mx xs = maybe xs (: xs) mx