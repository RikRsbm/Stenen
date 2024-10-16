module Functionality where

import Model
    ( Movable(steer, location, velocity, radius),
      TempObject(..),
      Bullet(Bullet),
      Steen(Steen),
      Player(..),
      GameState( player, aliens),
      Alien, CanShoot (shootBullet) )
import General ( addVec )
import Constants
    ( inputAccelPlayer,
      inputSteerPlayer,
      halfWidthFloat,
      halfHeightFloat, alienBulletOdds )
import Graphics.Gloss.Data.Vector (mulSV, magV)
import System.Random (mkStdGen, Random (randomR))





boost :: Player -> Player
boost p@(Player { pVelocity = vec }) 
    = p { pVelocity = vec `addVec` mulSV inputAccelPlayer (lookDirection p) }

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

checkMovementKeyPressed :: (Char, Bool) -> Player -> Player
checkMovementKeyPressed ('w', True) p = boost p
checkMovementKeyPressed ('a', True) p = steer p inputSteerPlayer 
checkMovementKeyPressed ('d', True) p = steer p (- inputSteerPlayer)
checkMovementKeyPressed _ gstate = gstate


newBullet :: GameState -> Int -> Maybe Bullet
newBullet gstate r 
    | i < length as = Just (shootBullet (as!!i) gstate)
    | otherwise     = Nothing
  where
    as = aliens gstate
    (i, _) = randomR (0, alienBulletOdds) (mkStdGen r)
    -- every alien has 1 / 100 probability to shoot