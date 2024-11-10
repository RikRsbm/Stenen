module TempObjectClass where




import DataTypes ( Bullet, Alien, Steen )
import Constants ( screenWidth, screenHeight )
import MovableClass ( Movable(location, updateLocation, radius) )



-- this class generalizes the temporary objects



class Movable a => TempObject a where
    updateLocations :: Float -> [a] -> [a] -- updates the locations of the objects in the list. Deletes an object if it goes out of bounds
    updateLocations secs = map (updateLocation secs) . filter checkWithinBounds

    checkWithinBounds :: a -> Bool -- checks whether object is within bounds
    checkWithinBounds m = x < halfWidth  && x > - halfWidth &&
                          y < halfHeight && y > - halfHeight
      where 
        r = radius m
        (x, y) = location m
        halfWidth  = fromIntegral (screenWidth `div` 2)  + r + 1 -- +1 so that it doesn't flag freshly spawned objects as out of bounds
        halfHeight = fromIntegral (screenHeight `div` 2) + r + 1

instance TempObject Steen where

instance TempObject Bullet where

instance TempObject Alien where