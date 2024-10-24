module Functionality where

import Model
import General 
import Constants
import Graphics.Gloss.Data.Vector
import System.Random 





boost :: Player -> Player
boost p@(Player { pVelocity = vec }) 
    = p { pVelocity = vec `addVec` mulSV inputAccelPlayer (lookDirection p), 
          boostState = case boostState p of 
              NotBoosting -> BoostFrame Zero2 0 
              _           -> boostState p}

pCheckBounds :: Player -> Player -- checks whether player goes out of bounds. if so, reset player within bounds
pCheckBounds p@(Player { pLocation = (x, y), pVelocity = (dx, dy) }) 
    = p { pLocation = (x', y'), pVelocity = (dx', dy') }
  where
    (x', dx') | x >   width  = (  width , 0 )
              | x < - width  = (- width , 0 ) 
              | otherwise    = (  x     , dx)
    (y', dy') | y >   height = (  height, 0 )
              | y < - height = (- height, 0 )
              | otherwise    = (  y     , dy)
    width  = halfWidthFloat - radius p
    height = halfHeightFloat - radius p

updateBoostAnimation :: Float -> Player -> Player
updateBoostAnimation secs p@(Player { boostState = BoostFrame x time }) 
    | time + secs > timePerBoostFrame = p { boostState = BoostFrame (case x of 
                                                                     Two2 -> Zero2
                                                                     other -> succ other) 
                                                         (time + secs - timePerBoostFrame) }
    | otherwise                       = p { boostState = BoostFrame x (time + secs) }
updateBoostAnimation secs p = p -- not boosting

 
checkMovementKeyPressed :: (Char, Bool) -> Player -> Player
checkMovementKeyPressed ('w', True) p = boost p
checkMovementKeyPressed ('w', False) p@(Player { boostState = BoostFrame _ _ }) = p { boostState = NotBoosting }
checkMovementKeyPressed ('a', True) p = steer p inputSteerPlayer 
checkMovementKeyPressed ('d', True) p = steer p (- inputSteerPlayer)
checkMovementKeyPressed _ gstate = gstate

newBullet :: GameState -> Int -> Maybe Bullet
newBullet gstate r 
    | i < length as = Just (shootBullet (as!!i) gstate)
    | otherwise     = Nothing
  where
    as = filter ((== Alive) . aState) (aliens gstate)
    (i, _) = randomR (0, alienBulletOdds) (mkStdGen r)
    -- every alien has 1 / alienBulletsOdds probability to shoot (max 1 per function call)

steer :: Player -> Float -> Player
steer p angle = p { lookDirection = rotateV angle (lookDirection p) } -- steer lookDirection 'angle' degrees in direction 'd'

pAutoDecceleration :: Player -> Player
pAutoDecceleration p@(Player { pVelocity = vec }) = p { pVelocity = newVec }
  where
    newVec | magV vec < autoDecelPlayer = (0, 0) -- if player (almost) stands  still
           | otherwise                  = vec `subVec` mulSV autoDecelPlayer (normalizeV vec) -- decelleration

