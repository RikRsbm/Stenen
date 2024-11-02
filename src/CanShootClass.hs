{-# LANGUAGE InstanceSigs #-}
module CanShootClass where



import MovableClass
import DataTypes
import Constants
import General
import Graphics.Gloss.Data.Vector



class CanShoot a where
    shootBullet :: Movable b => a -> b -> Bullet -- a shoots bullet at b

instance CanShoot Player where
    shootBullet :: Movable b => Player -> b -> Bullet
    shootBullet p _ = Bullet loc vec (pColor p)
      where 
        loc = location p `addVecToPt` (playerRadius `mulSV` lookDirection p) -- make sure bullet starts at point of player
        vec = (playerBulletSpeed `mulSV` lookDirection p) `addVec` velocity p

instance CanShoot Alien where
    shootBullet :: Movable b => Alien -> b -> Bullet
    shootBullet a p = Bullet loc vec alienColor
      where
        loc = location a `addVec` ((radius a / 2) `mulSV` normalizeV vec)
        vec = alienBulletSpeed `mulSV` normalizeV (location p `subVec` location a)