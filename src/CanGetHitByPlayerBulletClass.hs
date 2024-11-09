{-# LANGUAGE InstanceSigs #-}
module CanGetHitByPlayerBulletClass where




import HasAnimationClass
import DataTypes
import Graphics.Gloss.Data.Vector
import MovableClass
import General
import HasImplosionAnimationClass



-- this class generalizes the objects that can get bit by a player bullet (which triggers an implosion animation)



class HasImplosionAnimation a => CanGetHitByPlayerBullet a where
    bColliding :: a -> Bullet -> Bool -- is this object currently colliding with a (player) bullet?
    
    checkBulletHitsAndUpdateAnims :: Float -> GameState -> [a] -> ([a], Int) -- starts/updates implosion animations if necessary
    checkBulletHitsAndUpdateAnims secs gstate xs = (alive ++ wereAlreadyDying' ++ newDying', length newDying)
      where 
        (wereAlive, wereAlreadyDying) = partition ((== Alive) . dieState) xs                                -- split on whether they were alive/dying BEFORE this frame
        (newDying, alive) = partition (\x -> any (bColliding x) (bullets gstate)) wereAlive                 -- split the alive on whether they get hit this frame or not
        wereAlreadyDying' = filter ((/= Dead) . dieState) $ map (updateImplosionAnim secs) wereAlreadyDying -- update implosion animations, and remove objects from the game if their implosion animation has finished
        newDying' = map (updateImplosionAnim secs) newDying                                                 -- start the implosion animations. These are the ones that got shot this frame 

        partition p xs = (filter p xs, filter (not . p) xs)

instance CanGetHitByPlayerBullet Steen where
    bColliding :: Steen -> Bullet -> Bool
    bColliding s b = pointsWithinDistance (location s) (location b) (radius s + radius b)

instance CanGetHitByPlayerBullet Alien where
    bColliding :: Alien -> Bullet -> Bool
    bColliding a b = pointsWithinDistance (location a) (location b) (radius a / 2 + radius b)
        -- /2 so that the bullet has to hit (roughly) the dome of the ufo, you can't shoot over or under the ufo and still hit it