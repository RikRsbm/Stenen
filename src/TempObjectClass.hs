module TempObjectClass where




import DataTypes
import Constants
import MovableClass




class Movable a => TempObject a where
    updateLocations :: Float -> [a] -> [a]
    updateLocations secs = map (glide secs) . filter checkWithinBounds

    checkWithinBounds :: a -> Bool
    checkWithinBounds m = x < halfWidth  && x > - halfWidth &&
                          y < halfHeight && y > - halfHeight
      where 
        r = radius m
        (x, y) = location m
        halfWidth  = fromIntegral (screenWidth `div` 2)  + r + 1 -- +1 so spawned stones don't immediately despawn
        halfHeight = fromIntegral (screenHeight `div` 2) + r + 1

instance TempObject Steen where

instance TempObject Bullet where

instance TempObject Alien where