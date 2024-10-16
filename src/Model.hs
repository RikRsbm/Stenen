{-# LANGUAGE InstanceSigs #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Constants
    ( lookDirectionVecMagnitude, bulletRadius, autoDecelPlayer, playerRadius, playerBulletSpeed, alienBulletSpeed, halfHeightFloat, halfWidth, halfWidthFloat )
import Graphics.Gloss ( Point, Vector )
import Graphics.Gloss.Data.Vector (magV, mulSV, rotateV, normalizeV)
import General ( subVec, addVecToPt, addVec )




data GameState = GameState {
                   player :: Player 
                 , stenen :: [Steen] -- list of onscreen asteroids
                 , bullets :: [Bullet] -- list of onscreen bullets
                 , aliens :: [Alien] -- list of onscreen aliens
                 , alienBullets :: [Bullet] -- list of onscreen bullets from the alien
                 , wPressed :: Bool -- is 'w' pressed
                 , aPressed :: Bool -- is 'a' pressed?
                 , dPressed :: Bool -- is 'd' pressed?
                 , status :: Status -- status of game
                 , score :: Int -- current score
                 , highscore :: Int -- all time highscore (gets loaded in at start of game)
                 }

initialState :: GameState
initialState = GameState (Player (0, 0) 
                                 (0, 0) 
                                 (0, lookDirectionVecMagnitude)
                         ) 
                         []
                         []
                         []
                         []
                         False
                         False
                         False
                         FirstStep
                         0
                         0




data Status = FirstStep -- the first step of the game, it then reads the highscore from highscore.txt
            | PreStart -- between FirstStep and the first 'w' press
            | Running -- while the game is running
            | Paused -- while the game is paused
            | GameOver -- when the game is over
            deriving Eq


data Player = Player { 
                pLocation :: Point -- location of player
              , pVelocity :: Vector -- velocity of player
              , lookDirection :: Vector -- direction that player is looking in, it's a vector of constant magnitude
              } 

data Steen = Steen { 
               sLocation :: Point -- location of asteroid
             , sVelocity :: Vector -- velocity of asteroid
             , sRadius :: Float -- radius of asteroid
             } 

data Alien = Alien {
               aLocation :: Point
             , aVelocity :: Vector
             }

data Bullet = Bullet {
                bLocation :: Point -- location of bullet
              , bVelocity :: Vector -- velocity of bullet
              }

class Movable a => IsRound a where
    radius :: a -> Float

    pColliding :: Player -> a -> Bool
    pColliding p s  
        | magV (x - a, y - b) < radius s + playerRadius / 2 = True -- /2 so that you actually have to touch the stone if you are sideways
        | otherwise                                         = False
      where 
        (x, y) = location p
        (a, b) = location s

    checkWithinBounds :: a -> Bool
    checkWithinBounds m = x < width  && x > - width &&
                          y < height && y > - height
      where 
        r = radius m
        (x, y) = location m
        width  = halfWidthFloat  + r + 1 -- +1 so spawned stones don't immediately despawn
        height = halfHeightFloat + r + 1

instance IsRound Steen where
    radius :: Steen -> Float
    radius = sRadius

instance IsRound Bullet where
    radius :: Bullet -> Float
    radius b = bulletRadius

class CanShoot a where
    shootBullet :: a -> GameState -> GameState

instance CanShoot Player where
    shootBullet :: Player -> GameState -> GameState
    shootBullet p gstate = gstate { bullets = bul : bullets gstate, score = score gstate - 1 }
      where 
        bul = Bullet loc vec
        loc = location p `addVecToPt` (playerRadius `mulSV` lookDirection p) -- make sure bullet starts at point of player
        vec = (playerBulletSpeed `mulSV` lookDirection p ) `addVec` velocity p

instance CanShoot Alien where
    shootBullet :: Alien -> GameState -> GameState
    shootBullet a gstate = gstate { alienBullets = bul : alienBullets gstate }
      where
        bul = Bullet loc vec 
        loc = undefined
        vec = alienBulletSpeed `mulSV` normalizeV (location (player gstate) `subVec` location a) 

class Movable a where -- things on screen that can move
    location :: a -> Point
    velocity :: a -> Vector
    glide :: a -> a -- how they should glide through space every step
    steer :: a -> Float -> a -- steers them by certain number of degrees

instance Movable Player where
    location :: Player -> Point
    location = pLocation

    velocity :: Player -> Vector
    velocity = pVelocity

    glide :: Player -> Player
    glide p@(Player { pLocation = (x, y), pVelocity = vec@(dx, dy) }) -- updates player position and velocity
        = p { pLocation = (x + dx, y + dy), pVelocity = newVec }
      where 
        newVec | magV vec < autoDecelPlayer = (0, 0) -- if player (almost) stands  still
               | otherwise                  = vec `subVec` mulSV autoDecelPlayer (normalizeV vec) -- decelleration

    steer :: Player -> Float -> Player
    steer p angle = p { lookDirection = rotateV angle (lookDirection p) } -- steer lookDirection 'angle' degrees in direction 'd'

instance Movable Steen where
    location :: Steen -> Point
    location = sLocation

    velocity :: Steen -> Vector
    velocity = sVelocity

    glide :: Steen -> Steen
    glide s@(Steen { sLocation = (x, y), sVelocity = (dx, dy) }) 
        = s { sLocation = (x + dx, y + dy) }

    steer :: Steen -> Float -> Steen -- not used yet
    steer s angle = s { sVelocity = rotateV angle (velocity s) } -- steer velocity 'angle' degrees in direction 'd'

instance Movable Bullet where
    location :: Bullet -> Point
    location = bLocation

    velocity :: Bullet -> Vector
    velocity = bVelocity

    glide :: Bullet -> Bullet
    glide b@(Bullet { bLocation = (x, y), bVelocity = (dx, dy) }) 
        = b { bLocation = (x + dx, y + dy) }

    steer :: Bullet -> Float -> Bullet -- not used yet, might use it in future
    steer b angle = b { bVelocity = rotateV angle (velocity b) } -- steer velocity 'angle' degrees in direction 'd'

instance Movable Alien where
    location :: Alien -> Point
    location = aLocation

    velocity :: Alien -> Vector
    velocity = aVelocity

    glide :: Alien -> Alien
    glide a@(Alien { aLocation = (x, y), aVelocity = (dx, dy) }) 
        = a { aLocation = (x + dx, y + dy) }

    steer :: Alien -> Float -> Alien -- not used yet, miht use it in future
    steer a angle = a { aVelocity = rotateV angle (velocity a) } -- steer 'angle' degrees in direction 'd'






