{-# LANGUAGE InstanceSigs #-}
module Model where

import Constants
import Graphics.Gloss
import Graphics.Gloss.Data.Vector 
import General 
import System.Random 




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
                 , ufoPic :: Picture
                 }

initialState :: Picture -> GameState
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

class Movable a => TempObject a where
    updateLocations :: [a] -> [a]
    updateLocations = map glide . filter checkWithinBounds

    pColliding :: Player -> a -> Bool
    pColliding p m  
        | magV (x - a, y - b) < radius m + playerRadius / 2 = True -- /2 so that you actually have to touch the stone / bullet / alien if you are sideways
        | otherwise                                         = False
      where 
        (x, y) = location p
        (a, b) = location m

    checkWithinBounds :: a -> Bool
    checkWithinBounds m = x < width  && x > - width &&
                          y < height && y > - height
      where 
        r = radius m
        (x, y) = location m
        width  = halfWidthFloat  + r + 1 -- +1 so spawned stones don't immediately despawn
        height = halfHeightFloat + r + 1

instance TempObject Steen where

instance TempObject Bullet where

instance TempObject Alien where

class CanShoot a where
    shootBullet :: a -> GameState -> Bullet

instance CanShoot Player where
    shootBullet :: Player -> GameState -> Bullet
    shootBullet p _ = Bullet loc vec
      where 
        loc = location p `addVecToPt` (playerRadius `mulSV` lookDirection p) -- make sure bullet starts at point of player
        vec = (playerBulletSpeed `mulSV` lookDirection p ) `addVec` velocity p

instance CanShoot Alien where
    shootBullet :: Alien -> GameState -> Bullet
    shootBullet a gstate = Bullet loc vec
      where
        loc = location a `addVec` ((radius a / 2) `mulSV` normalizeV vec)
        vec = alienBulletSpeed `mulSV` normalizeV (location (player gstate) `subVec` location a) 

class Movable a where -- things on screen that can move
    radius :: a -> Float
    location :: a -> Point
    velocity :: a -> Vector
    glide :: a -> a -- how they should glide through space every step
    steer :: a -> Float -> a -- steers them by certain number of degrees

instance Movable Player where
    radius :: Player -> Float
    radius p = playerRadius

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
    radius :: Steen -> Float
    radius = sRadius 

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
    radius :: Bullet -> Float
    radius b = bulletRadius

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
    radius :: Alien -> Float
    radius a = alienRadius

    location :: Alien -> Point
    location = aLocation

    velocity :: Alien -> Vector
    velocity = aVelocity

    glide :: Alien -> Alien
    glide a@(Alien { aLocation = (x, y), aVelocity = (dx, dy) }) 
        = a { aLocation = (x + dx, y + dy) }

    steer :: Alien -> Float -> Alien -- not used yet, miht use it in future
    steer a angle = a { aVelocity = rotateV angle (velocity a) } -- steer 'angle' degrees in direction 'd'

class TempObject a => RandomObject a where
    perhapsCreateNew :: GameState -> Int -> Maybe a

    updateRemoveAndAdd :: GameState -> Int -> [a] -> ([a], Int)
    updateRemoveAndAdd gstate r as = (addMaybe (perhapsCreateNew gstate r) (updateLocations nonCollidedAs), 
                                      length as - length nonCollidedAs)
      where nonCollidedAs = filter (\a -> not (any (bColliding a) (bullets gstate))) as

    bColliding :: a -> Bullet -> Bool 
    
instance RandomObject Steen where
    perhapsCreateNew :: GameState -> Int -> Maybe Steen
    perhapsCreateNew gstate seed 
        | creationOdds == 0 = Just (Steen (x, y) (dx, dy) r)
        | otherwise         = Nothing
      where
        gen = mkStdGen seed
        (creationOdds , gen1) = randomR (0  :: Int            , 100                ) gen 
        (radius      , gen2) = randomR (15 :: Int            , 40                 ) gen1
        (randomX     , gen3) = randomR (- halfWidth  - radius, halfWidth  + radius) gen2
        (randomY     , gen4) = randomR (- halfHeight - radius, halfHeight + radius) gen3
        (bigVelocity , gen5) = randomR (10  :: Int           , 20                 ) gen4
        (side        , _   ) = randomR (0  :: Int            , 3                  ) gen5

        (x, y) = case side of -- pick a side
                  0 -> (fromIntegral randomX, - halfHeightFloat - r)
                  1 -> (fromIntegral randomX,   halfHeightFloat + r)
                  2 -> (- halfWidthFloat - r, fromIntegral randomY)
                  _ -> (halfWidthFloat   + r, fromIntegral randomY)

        (dx, dy) = v `mulSV` normalizeV (a - x, b - y)
        (a, b) = location (player gstate)
        v = fromIntegral bigVelocity / 10
        r = fromIntegral radius

    bColliding :: Steen -> Bullet -> Bool
    bColliding s b  
        | magV (x - p, y - q) < radius s + radius b = True
        | otherwise                                 = False
      where 
        (x, y) = location b
        (p, q) = location s

instance RandomObject Alien where -- we don't use gstate yet, we might in the future (so that it can move towards the player or something like that)
    perhapsCreateNew :: GameState -> Int -> Maybe Alien
    perhapsCreateNew _ seed 
        | creationOdds == 0 = Just (Alien (x, y) (dx, dy))
        | otherwise         = Nothing
      where
        gen = mkStdGen seed
        (creationOdds, gen1) = randomR (0  :: Int       , alienOdds     ) gen 
        (randomX     , gen2) = randomR (- halfWidth  - r, halfWidth  + r) gen1
        (randomY     , gen3) = randomR (- halfHeight - r, halfHeight + r) gen2
        (side        , _   ) = randomR (0  :: Int       , 3             ) gen3

        (x, y, dx, dy) = case side of -- pick a side
                          0 -> (fromIntegral randomX          , - halfHeightFloat - alienRadius, 0           , alienSpeed  ) 
                          1 -> (fromIntegral randomX          ,   halfHeightFloat + alienRadius, 0           , - alienSpeed)
                          2 -> (- halfWidthFloat - alienRadius, fromIntegral randomY           , alienSpeed  , 0           )
                          _ -> (halfWidthFloat   + alienRadius, fromIntegral randomY           , - alienSpeed, 0           )

        r = round alienRadius

    bColliding :: Alien -> Bullet -> Bool
    bColliding a b  
        | magV (x - p, y - q) < radius a / 2 + radius b = True -- /2 so that you have to hit the center of the alien, you can shoot over or under it and hit it
        | otherwise                                     = False
      where 
        (x, y) = location b
        (p, q) = location a







