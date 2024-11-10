{-# LANGUAGE InstanceSigs #-}
module MovableClass where




import Graphics.Gloss
import DataTypes
import Constants


    

-- this class generalizes the objects that can move




class Movable a where 
    radius :: a -> Float 
    location :: a -> Point
    velocity :: a -> Vector -- velocity of object, in pixels/game tick
    replaceLocation :: a -> Point -> a -- replaces the location of the object by the provided location

    updateLocation :: Float -> a -> a -- updates the location to where the object should be at this point in time
    updateLocation secs a = replaceLocation a (x + dx * gameTicksPerSec * secs, y + dy * gameTicksPerSec * secs)
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