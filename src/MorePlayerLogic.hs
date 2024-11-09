module MorePlayerLogic where



import DataTypes
import General 
import Constants
import Graphics.Gloss.Data.Vector
import System.Random 
import MovableClass
import CanCollideWithPlayerClass
import CanShootClass
import HasImplosionAnimationClass




-- this module contains the logic that is specific to the Player datatype



steer :: Player -> Float -> Player -- steer the player 'angle' radians counterclockwise
steer p angle = p { lookDirection = rotateV angle (lookDirection p) } 




autoDecceleration :: Player -> Player -- apply automatic decceleration to the player
autoDecceleration p@(Player { pVelocity = vec }) = p { pVelocity = newVec }
  where
    newVec | magV vec < autoDecelPlayer = (0, 0) -- if player almost stands still, set his velocity to 0
           | otherwise                  = vec `subVec` mulSV autoDecelPlayer (normalizeV vec) 




boost :: Player -> Player -- apply the standard amount of acceleration to the player (the player is currently pressing the boost key)
boost p@(Player { pVelocity = vec }) 
    = p { pVelocity = vec `addVec` mulSV inputAccelPlayer (lookDirection p) }




playerShoots :: GameState -> Player -> GameState -- the player shoots, so create a new bullet and add it to the gamestate. subtract 1 point from the score
playerShoots gstate p = gstate { bullets = bul : bullets gstate, score = score gstate - 1 }
  where bul = shootBullet p p -- second argument doesnt matter, since player doesnt shoot *at* something, 
                              -- but rather in the direction he is pointed 





pCheckBounds :: Player -> Player -- checks whether player goes out of bounds. if so, reset player within bounds
pCheckBounds p@(Player { pLocation = (x, y), pVelocity = (dx, dy) }) 
    = p { pLocation = (x', y'), pVelocity = (dx', dy') }
  where
    (x', dx') | x >   halfWidth  = (  halfWidth , 0 )
              | x < - halfWidth  = (- halfWidth , 0 ) 
              | otherwise        = (  x         , dx)
    (y', dy') | y >   halfHeight = (  halfHeight, 0 )
              | y < - halfHeight = (- halfHeight, 0 )
              | otherwise        = (  y         , dy)

    halfWidth  = fromIntegral (screenWidth `div` 2) - radius p
    halfHeight = fromIntegral (screenHeight `div` 2) - radius p
 




aPlayerHitsSomething :: GameState -> Bool -- check whether a player hits something (and therefore dies)
aPlayerHitsSomething gstate = thisPlayerHitsSomething gstate (player gstate) ||             -- if player1 hits something
                              maybe False (thisPlayerHitsSomething gstate) (player2 gstate) -- if player2 exists and hits something

thisPlayerHitsSomething :: GameState -> Player -> Bool -- check whether this player hits something
thisPlayerHitsSomething gstate p = collidesWith (alive (stenen gstate)) || -- if player hits an alive steen
                                   collidesWith (alive (aliens gstate)) || -- if player hits an alive alien
                                   collidesWith (alienBullets gstate)      -- if an alien bullet hits the player
  where
    alive :: HasImplosionAnimation a => [a] -> [a]
    alive = filter ((== Alive) . dieState)

    collidesWith :: CanCollideWithPlayer a => [a] -> Bool
    collidesWith = any (pColliding p)  





pickPlayer :: GameState -> StdGen -> (Player, StdGen) -- randomly picks player1 or player2 (if player2 doesn't exist, it picks player1). also updates the generator
pickPlayer gstate gen = (case player2 gstate of
                         Just pl2 -> if i == 1 then player gstate else pl2
                         _        -> player gstate
                       , newGen)
  where
    (i, newGen) = randomR (1 :: Int, 2) gen





checkMovementKeysPressed :: Player -> Player -- checks whether player wants to boost or steer, handles accordingly
checkMovementKeysPressed p = foldr checkMovementKeyPressed p keysPressed
  where 
    keysPressed = [(Boost,      boostState p /= NotBoosting),        -- this list says which movement keys the player is currently pressing
                   (SteerLeft,  leftPressed p), 
                   (SteerRight, rightPressed p)]

checkMovementKeyPressed :: (MovementKeys, Bool) -> Player -> Player -- call the appropriate functions to boost or steer if the player wants to
checkMovementKeyPressed (Boost,      True) p = boost p
checkMovementKeyPressed (SteerLeft,  True) p = steer p inputSteerPlayer 
checkMovementKeyPressed (SteerRight, True) p = steer p (- inputSteerPlayer)
checkMovementKeyPressed _ gstate = gstate