{-# LANGUAGE InstanceSigs #-}
module CanCollideWithPlayerClass where




import MovableClass
import DataTypes
import Graphics.Gloss.Data.Vector
import Constants
import General 



-- this type class generalizes objects that can collide with the player



class Movable m => CanCollideWithPlayer m where
    pColliding :: Player -> m -> Bool -- are player and 'm' currently colliding?
    pColliding p m = pointsWithinDistance (location p) (location m) (radius m + playerRadius / 2)
        -- /2 so that the player actually has to touch the steen / bullet / alien if the player is sideways

instance CanCollideWithPlayer Steen where

instance CanCollideWithPlayer Bullet where

instance CanCollideWithPlayer Alien where
    pColliding :: Player -> Alien -> Bool
    pColliding p m = pointsWithinDistance (location p) (location m) (radius m / 2 + playerRadius / 2)  
        -- the left /2 is so that the player actually has to hit the alien when the player approaches from above or beneath


