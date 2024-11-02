module Functionality where

import Model
import General 
import Constants
import Graphics.Gloss.Data.Vector
import System.Random 
import Data.Maybe (fromMaybe)
import Graphics.Gloss (Point)




steer :: Player -> Float -> Player
steer p angle = p { lookDirection = rotateV angle (lookDirection p) } -- steer lookDirection 'angle' degrees in direction 'd'

pAutoDecceleration :: Player -> Player
pAutoDecceleration p@(Player { pVelocity = vec }) = p { pVelocity = newVec }
  where
    newVec | magV vec < autoDecelPlayer = (0, 0) -- if player (almost) stands  still
           | otherwise                  = vec `subVec` mulSV autoDecelPlayer (normalizeV vec) -- decelleration

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
 

checkMovementKeysPressed :: Player -> Player
checkMovementKeysPressed p = foldr checkMovementKeyPressed p keysPressed
  where 
    keysPressed = [(Forward, forwardPressed p), (Model.Left, leftPressed p), (Model.Right, rightPressed p)]

checkMovementKeyPressed :: (Direction, Bool) -> Player -> Player
checkMovementKeyPressed (Forward, True) p = boost p
checkMovementKeyPressed (Forward, False) p@(Player { boostState = BoostFrame _ _ }) = p { boostState = NotBoosting }
checkMovementKeyPressed (Model.Left, True) p = steer p inputSteerPlayer 
checkMovementKeyPressed (Model.Right, True) p = steer p (- inputSteerPlayer)
checkMovementKeyPressed _ gstate = gstate




aPlayerHitsSomething :: GameState -> Bool
aPlayerHitsSomething gstate = thisPlayerHitsSomething gstate (player gstate) ||
                              case player2 gstate of
                              Just p -> thisPlayerHitsSomething gstate p
                              _      -> False

thisPlayerHitsSomething :: GameState -> Player -> Bool
thisPlayerHitsSomething gstate p = any (pColliding p) (filter ((== Alive) . sState) (stenen gstate)) ||
                                   any (pColliding p) (filter ((== Alive) . aState) (aliens gstate)) ||
                                   any (pColliding p) (alienBullets gstate)





withinButtonBounds :: Point -> Button -> Bool
withinButtonBounds (x,y) but = x > x' - halfW && x < x' + halfW
                            && y > y' - halfH && y < y' + halfH
  where 
    (x', y') = butLocation but
    (halfW, halfH) = 0.5 `mulSV` butSize but 





addPlayerBullet :: Player -> GameState -> GameState
addPlayerBullet p gstate = gstate { bullets = bul : bullets gstate, score = score gstate - 1 }
  where bul = shootBullet p p -- second argument doesnt matter, since player doesnt shoot *at* something, 
                              -- but rather in the direction he is pointed 