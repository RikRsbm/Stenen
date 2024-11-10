{-# LANGUAGE InstanceSigs #-}
module CanShootClass where



import MovableClass
import DataTypes
import Constants
import General
import Graphics.Gloss.Data.Vector



-- this class generalizes the objects that can shoot a bullet



class CanShoot a where
    shootBullet :: Movable b => a -> b -> Bullet -- a shoots bullet at b, give that bullet as output

instance CanShoot Player where
    shootBullet :: Movable b => Player -> b -> Bullet -- player shoots bullet 
    shootBullet p _ = Bullet loc vec (pColor p) -- an object of type b is not needed, since the player doesn't shoot *at* something, but rather in a direction
      where 
        loc = location p `addVec` (playerRadius `mulSV` lookDirection p) -- starting position of bullet. this line makes sure the bullet starts at the front end of the player
        vec = (playerBulletSpeed `mulSV` lookDirection p) `addVec` velocity p -- velocity of the bullet (it gets the player's velocity on top of the standard bullet speed)

instance CanShoot Alien where
    shootBullet :: Movable b => Alien -> b -> Bullet -- alien shoots bullet at player
    shootBullet a p = Bullet loc vec alienColor
      where
        loc = location a `addVec` ((radius a / 2) `mulSV` normalizeV vec) -- starting position of bullet. this line makes sure the bullet starts at the edge of the ufo dome
        vec = alienBulletSpeed `mulSV` normalizeV (location p `subVec` location a) -- velocity of the bullet (standard speed, towards current player location)