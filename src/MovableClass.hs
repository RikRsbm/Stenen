{-# LANGUAGE InstanceSigs #-}
module MovableClass where




import Graphics.Gloss
import DataTypes
import Constants


    




class Movable a where -- things on screen that can move
    radius :: a -> Float
    location :: a -> Point
    velocity :: a -> Vector
    glide :: Float -> a -> a -- how they should glide through space every step

instance Movable Player where
    radius :: Player -> Float
    radius p = playerRadius

    location :: Player -> Point
    location = pLocation

    velocity :: Player -> Vector
    velocity = pVelocity

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

    glide :: Float -> Steen -> Steen
    glide secs s@(Steen { sLocation = (x, y), sVelocity = (dx, dy) }) 
        = s { sLocation = (x + dx * secs, y + dy * secs) }

instance Movable Bullet where
    radius :: Bullet -> Float
    radius b = bulletRadius

    location :: Bullet -> Point
    location = bLocation

    velocity :: Bullet -> Vector
    velocity = bVelocity

    glide :: Float -> Bullet -> Bullet
    glide secs b@(Bullet { bLocation = (x, y), bVelocity = (dx, dy) }) 
        = b { bLocation = (x + dx * secs, y + dy * secs) }

instance Movable Alien where
    radius :: Alien -> Float
    radius a = alienRadius

    location :: Alien -> Point
    location = aLocation

    velocity :: Alien -> Vector
    velocity = aVelocity

    glide :: Float -> Alien -> Alien
    glide secs a@(Alien { aLocation = (x, y), aVelocity = (dx, dy) }) 
        = a { aLocation = (x + dx * secs, y + dy * secs) }