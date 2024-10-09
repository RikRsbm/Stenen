{-# LANGUAGE InstanceSigs #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Constants
    ( lookDirectionVecMagnitude, bulletRadius, autoDecelPlayer )
import Graphics.Gloss ( Point, Vector )
import Graphics.Gloss.Data.Vector (magV, mulSV, rotateV, normalizeV)
import General ( subVec )




data GameState = GameState {
                   player :: Player 
                 , stenen :: [Steen] -- list of onscreen asteroids
                 , bullets :: [Bullet] -- list of onscreen bullets
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

data Bullet = Bullet {
                bLocation :: Point -- location of bullet
              , bVelocity :: Vector -- velocity of bullet
              }

class IsRound a where
    radius :: a -> Float

instance IsRound Steen where
    radius :: Steen -> Float
    radius = sRadius

instance IsRound Bullet where
    radius :: Bullet -> Float
    radius b = bulletRadius

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
    steer p angle = p { lookDirection = rotateV angle (lookDirection p) } -- steer 'angle' degrees in direction 'd'

instance Movable Steen where
    location :: Steen -> Point
    location = sLocation

    velocity :: Steen -> Vector
    velocity = sVelocity

    glide :: Steen -> Steen
    glide s@(Steen { sLocation = (x, y), sVelocity = (dx, dy) }) 
        = s { sLocation = (x + dx, y + dy) }

    steer :: Steen -> Float -> Steen -- not used yet
    steer s angle = s { sVelocity = rotateV angle (velocity s) } -- steer 'angle' degrees in direction 'd'

instance Movable Bullet where
    location :: Bullet -> Point
    location = bLocation

    velocity :: Bullet -> Vector
    velocity = bVelocity

    glide :: Bullet -> Bullet
    glide b@(Bullet { bLocation = (x, y), bVelocity = (dx, dy) }) 
        = b { bLocation = (x + dx, y + dy) }

    steer :: Bullet -> Float -> Bullet -- not used yet
    steer b angle = b { bVelocity = rotateV angle (velocity b) } -- steer 'angle' degrees in direction 'd'






