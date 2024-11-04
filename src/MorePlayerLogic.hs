module MorePlayerLogic where



import DataTypes
import General 
import Constants
import Graphics.Gloss.Data.Vector
import System.Random 
import MovableClass
import CanCollideWithPlayerClass
import CanShootClass




steer :: Player -> Float -> Player
steer p angle = p { lookDirection = rotateV angle (lookDirection p) } -- steer lookDirection 'angle' degrees in direction 'd'




autoDecceleration :: Player -> Player
autoDecceleration p@(Player { pVelocity = vec }) = p { pVelocity = newVec }
  where
    newVec | magV vec < autoDecelPlayer = (0, 0) -- if player (almost) stands  still
           | otherwise                  = vec `subVec` mulSV autoDecelPlayer (normalizeV vec) -- decelleration




boost :: Player -> Player
boost p@(Player { pVelocity = vec }) 
    = p { pVelocity = vec `addVec` mulSV inputAccelPlayer (lookDirection p), 
          boostState = case boostState p of 
              NotBoosting -> BoostFrame Zero2 0 
              _           -> boostState p}




playerShoots :: GameState -> Player -> GameState
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
 




aPlayerHitsSomething :: GameState -> Bool
aPlayerHitsSomething gstate = thisPlayerHitsSomething gstate (player gstate) ||
                              case player2 gstate of
                              Just p -> thisPlayerHitsSomething gstate p
                              _      -> False

thisPlayerHitsSomething :: GameState -> Player -> Bool
thisPlayerHitsSomething gstate p = any (pColliding p) (filter ((== Alive) . sState) (stenen gstate)) ||
                                   any (pColliding p) (filter ((== Alive) . aState) (aliens gstate)) ||
                                   any (pColliding p) (alienBullets gstate)





pickPlayer :: GameState -> StdGen -> (Player, StdGen)
pickPlayer gstate gen = (case player2 gstate of
                         Just pl2 -> case i of
                                      1 -> player gstate
                                      _ -> pl2
                         _        -> player gstate
                       , newGen)
  where
    (i, newGen) = randomR (1 :: Int, 2) gen





checkMovementKeysPressed :: Player -> Player
checkMovementKeysPressed p = foldr checkMovementKeyPressed p keysPressed
  where 
    keysPressed = [(Forward, forwardPressed p), (DataTypes.Left, leftPressed p), (DataTypes.Right, rightPressed p)]

checkMovementKeyPressed :: (Direction, Bool) -> Player -> Player
checkMovementKeyPressed (Forward, True) p = boost p
checkMovementKeyPressed (Forward, False) p@(Player { boostState = BoostFrame _ _ }) = p { boostState = NotBoosting }
checkMovementKeyPressed (DataTypes.Left, True) p = steer p inputSteerPlayer 
checkMovementKeyPressed (DataTypes.Right, True) p = steer p (- inputSteerPlayer)
checkMovementKeyPressed _ gstate = gstate