{-# LANGUAGE InstanceSigs #-}
module CanGetHitByPlayerBulletClass where




import HasAnimationClass
import DataTypes
import Graphics.Gloss.Data.Vector
import MovableClass




class HasAnimation a => CanGetHitByPlayerBullet a where
    bColliding :: a -> Bullet -> Bool
    state :: a -> State
    
    checkBulletHitsAndUpdateAnims :: Float -> GameState -> [a] -> ([a], Int)
    checkBulletHitsAndUpdateAnims secs gstate xs = (alive ++ wereAlreadyDying' ++ newDying', length newDying)
      where 
        (wereAlive, wereAlreadyDying) = partition ((== Alive) . state) xs
        (newDying, alive) = partition (\x -> any (bColliding x) (bullets gstate)) wereAlive
        wereAlreadyDying' = filter ((/= Dead) . state) $ map (updateAnim secs) wereAlreadyDying
        newDying' = map (updateAnim secs) newDying

        partition p xs = (filter p xs, filter (not . p) xs)

instance CanGetHitByPlayerBullet Steen where
    bColliding :: Steen -> Bullet -> Bool
    bColliding s b  
        | magV (x - p, y - q) < radius s + radius b = True
        | otherwise                                 = False
      where 
        (x, y) = location b
        (p, q) = location s

    state :: Steen -> State
    state = sState

instance CanGetHitByPlayerBullet Alien where
    bColliding :: Alien -> Bullet -> Bool
    bColliding a b  
        | magV (x - p, y - q) < radius a / 2 + radius b = True -- /2 so that you have to hit the center of the alien, you can shoot over or under it and hit it
        | otherwise                                     = False
      where 
        (x, y) = location b
        (p, q) = location a

    state :: Alien -> State
    state = aState


