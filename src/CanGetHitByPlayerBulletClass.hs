{-# LANGUAGE InstanceSigs #-}
module CanGetHitByPlayerBulletClass where




import HasAnimationClass
import DataTypes
import Graphics.Gloss.Data.Vector
import MovableClass
import General
import HasImplosionAnimationClass



-- this type class generalizes objects that can get bit by a player bullet (which triggers an animation)



class HasImplosionAnimation a => CanGetHitByPlayerBullet a where
    bColliding :: a -> Bullet -> Bool -- is this object currently colliding with a (player) bullet?
    
    checkBulletHitsAndUpdateAnims :: Float -> GameState -> [a] -> ([a], Int)
    checkBulletHitsAndUpdateAnims secs gstate xs = (alive ++ wereAlreadyDying' ++ newDying', length newDying)
      where 
        (wereAlive, wereAlreadyDying) = partition ((== Alive) . dieState) xs
        (newDying, alive) = partition (\x -> any (bColliding x) (bullets gstate)) wereAlive
        wereAlreadyDying' = filter ((/= Dead) . dieState) $ map (updateImplosionAnim secs) wereAlreadyDying
        newDying' = map (updateImplosionAnim secs) newDying

        partition p xs = (filter p xs, filter (not . p) xs)

instance CanGetHitByPlayerBullet Steen where
    bColliding :: Steen -> Bullet -> Bool
    bColliding s b = pointsWithinDistance (location s) (location b) (radius s + radius b)

instance CanGetHitByPlayerBullet Alien where
    bColliding :: Alien -> Bullet -> Bool
    bColliding a b = pointsWithinDistance (location a) (location b) (radius a / 2 + radius b)
        -- /2 so that you have to hit the center of the alien, you can shoot over or under it and hit it