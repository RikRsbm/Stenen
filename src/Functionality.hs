module Functionality where

import Model
    ( Movable(steer, location, velocity),
      IsRound(..),
      Bullet(Bullet),
      Steen(Steen),
      Player(..),
      GameState( player), 
      Alien )
import General ( addVec )
import Constants
    ( playerRadius,
      inputAccelPlayer,
      inputSteerPlayer,
      halfWidth,
      halfWidthFloat,
      halfHeight,
      halfHeightFloat )
import Graphics.Gloss.Data.Vector (mulSV, normalizeV, magV)
import System.Random (Random(randomR), mkStdGen)





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
    width  = halfWidthFloat - playerRadius
    height = halfHeightFloat - playerRadius

sbColliding :: Steen -> Bullet -> Bool 
sbColliding b s  
    | magV (x - p, y - q) < radius s + radius b = True
    | otherwise                                 = False
  where 
    (x, y) = location b
    (p, q) = location s

alienShootBullet :: GameState -> Alien -> GameState
alienShootBullet = undefined

randomSteen :: Int -> GameState -> Maybe Steen
randomSteen seed gstate 
    | steenOdds == 0 = Just (Steen (x, y) (dx, dy) r)
    | otherwise      = Nothing
  where
    gen = mkStdGen seed
    (steenOdds  , gen1) = randomR (0  :: Int            , 100                ) gen 
    (radius     , gen2) = randomR (15 :: Int            , 40                 ) gen1
    (randomX    , gen3) = randomR (- halfWidth  - radius, halfWidth  + radius) gen2
    (randomY    , gen4) = randomR (- halfHeight - radius, halfHeight + radius) gen3
    (bigVelocity, gen5) = randomR (10  :: Int           , 20                 ) gen4
    (side       , gen6) = randomR (0  :: Int            , 3                  ) gen5

    (x, y) = case side of -- pick a side
               0 -> (fromIntegral randomX, - halfHeightFloat - r)
               1 -> (fromIntegral randomX,   halfHeightFloat + r)
               2 -> (- halfWidthFloat - r, fromIntegral randomY)
               _ -> (halfWidthFloat   + r, fromIntegral randomY)

    (dx, dy) = v `mulSV` normalizeV (a - x, b - y)
    (a, b) = location (player gstate)
    v = fromIntegral bigVelocity / 10
    r = fromIntegral radius

checkMovementKeyPressed :: (Char, Bool) -> Player -> Player
checkMovementKeyPressed ('w', True) p = boost p
checkMovementKeyPressed ('a', True) p = steer p inputSteerPlayer 
checkMovementKeyPressed ('d', True) p = steer p (- inputSteerPlayer)
checkMovementKeyPressed _ gstate = gstate