{-# LANGUAGE InstanceSigs #-}
module MovableClass where




import Graphics.Gloss
import DataTypes
import Constants


    




class Movable a where -- things on screen that can move
    radius :: a -> Float
    location :: a -> Point
    velocity :: a -> Vector
    replaceLocation :: a -> Point -> a

    glide :: Float -> a -> a -- how they should glide through space every step
    glide secs a = replaceLocation a (x + dx * secs, y + dy * secs)
      where
        (x, y) = location a
        (dx, dy) = velocity a

instance Movable Player where
    radius :: Player -> Float
    radius p = playerRadius

    location :: Player -> Point
    location = pLocation

    velocity :: Player -> Vector
    velocity = pVelocity

    replaceLocation :: Player -> Point -> Player
    replaceLocation p loc = p { pLocation = loc }

    glide :: Float -> Player -> Player
    glide secs p@(Player { pLocation = (x, y), pVelocity = (dx, dy) }) -- updates player position and velocity
        = p { pLocation = (x + dx * secs, y + dy * secs) }

instance Movable Steen where
    radius :: Steen -> Float
    radius = sRadius 

    location :: Steen -> Point
    location = sLocation

    velocity :: Steen -> Vector
    velocity = sVelocity

    replaceLocation :: Steen -> Point -> Steen
    replaceLocation s loc = s { sLocation = loc }

instance Movable Bullet where
    radius :: Bullet -> Float
    radius b = bulletRadius

    location :: Bullet -> Point
    location = bLocation

    velocity :: Bullet -> Vector
    velocity = bVelocity

    replaceLocation :: Bullet -> Point -> Bullet
    replaceLocation b loc = b { bLocation = loc }

instance Movable Alien where
    radius :: Alien -> Float
    radius a = alienRadius

    location :: Alien -> Point
    location = aLocation

    velocity :: Alien -> Vector
    velocity = aVelocity

    replaceLocation :: Alien -> Point -> Alien
    replaceLocation a loc = a { aLocation = loc }