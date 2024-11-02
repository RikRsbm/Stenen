{-# LANGUAGE InstanceSigs #-}
module CanCollideWithPlayerClass where




import MovableClass
import DataTypes
import Graphics.Gloss.Data.Vector
import Constants



class Movable a => CanCollideWithPlayer a where
    pColliding :: Player -> a -> Bool
    pColliding p m  
        | magV (x - a, y - b) < radius m + playerRadius / 2 = True -- /2 so that you actually have to touch the stone / bullet / alien if you are sideways
        | otherwise                                         = False
      where 
        (x, y) = location p
        (a, b) = location m

instance CanCollideWithPlayer Steen where

instance CanCollideWithPlayer Bullet where

instance CanCollideWithPlayer Alien where
    pColliding :: Player -> Alien -> Bool
    pColliding p al  
        | magV (x - a, y - b) < radius al / 2 + playerRadius / 2 = True -- the left /2 is so that you actually have to touch the alien when the alien is sideways
        | otherwise                                              = False
      where 
        (x, y) = location p
        (a, b) = location al