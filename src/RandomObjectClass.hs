{-# LANGUAGE InstanceSigs #-}
module RandomObjectClass where




import TempObjectClass
import DataTypes
import System.Random
import Constants
import Graphics.Gloss.Data.Vector
import MovableClass
import CanShootClass
import MorePlayerLogic





class (TempObject a) => RandomObject a where
    perhapsCreateNew :: GameState -> StdGen -> (Maybe a, StdGen)    
    
instance RandomObject Steen where
    perhapsCreateNew :: GameState -> StdGen -> (Maybe Steen, StdGen)
    perhapsCreateNew gstate gen 
        | creationOdds == 0 = (Just (Steen (x, y) (dx, dy) r Alive), newGen)
        | otherwise         = (Nothing, gen1)
      where
        (creationOdds, gen1  ) = randomR (0                    , steenCreationOdds  ) gen 
        (radius      , gen2  ) = randomR (steenMinRadius       , steenMaxRadius     ) gen1
        (randomX     , gen3  ) = randomR (- halfWidth  - radius, halfWidth  + radius) gen2
        (randomY     , gen4  ) = randomR (- halfHeight - radius, halfHeight + radius) gen3
        (speed       , gen5  ) = randomR (steenMinSpeed        , steenMaxSpeed      ) gen4
        (side        , gen6  ) = randomR (0  :: Int            , 3                  ) gen5
        (targetP     , newGen) = pickPlayer gstate gen6

        (x, y) = case side of -- pick a side
                  0 -> (fromIntegral randomX, - halfHeightFloat - r)
                  1 -> (fromIntegral randomX,   halfHeightFloat + r)
                  2 -> (- halfWidthFloat - r, fromIntegral randomY)
                  _ -> (  halfWidthFloat + r, fromIntegral randomY)

        (dx, dy) = fromIntegral speed `mulSV` normalizeV (a - x, b - y)
        (a, b) = location targetP
        r = fromIntegral radius

        halfWidth = screenWidth `div` 2
        halfHeight = screenHeight `div` 2
        halfWidthFloat = fromIntegral halfWidth
        halfHeightFloat = fromIntegral halfHeight

instance RandomObject Alien where -- we don't use gstate yet, we might in the future (so that it can move towards the player or something like that)
    perhapsCreateNew :: GameState -> StdGen -> (Maybe Alien, StdGen)
    perhapsCreateNew _ gen 
        | creationOdds == 0 = (Just (Alien (x, y) (dx, dy) Alive), newGen)
        | otherwise         = (Nothing, gen1)
      where
        (creationOdds, gen1  ) = randomR (0               , alienCreationOdds) gen 
        (randomX     , gen2  ) = randomR (- halfWidth  - r, halfWidth  + r   ) gen1
        (randomY     , gen3  ) = randomR (- halfHeight - r, halfHeight + r   ) gen2
        (side        , newGen) = randomR (0  :: Int       , 3                ) gen3

        (x, y, dx, dy) = case side of -- pick a side
                          0 -> (fromIntegral randomX          , - halfHeightFloat - alienRadius, 0           ,   alienSpeed) 
                          1 -> (fromIntegral randomX          ,   halfHeightFloat + alienRadius, 0           , - alienSpeed)
                          2 -> (- halfWidthFloat - alienRadius, fromIntegral randomY           ,   alienSpeed, 0           )
                          _ -> (  halfWidthFloat + alienRadius, fromIntegral randomY           , - alienSpeed, 0           )

        r = round alienRadius

        halfWidth = screenWidth `div` 2
        halfHeight = screenHeight `div` 2
        halfWidthFloat = fromIntegral halfWidth
        halfHeightFloat = fromIntegral halfHeight

instance RandomObject Bullet where
    perhapsCreateNew :: GameState -> StdGen -> (Maybe Bullet, StdGen) -- perhaps creates alien bullet
    perhapsCreateNew gstate gen 
        | i < length as = (Just (shootBullet (as!!i) targetP), newGen)
        | otherwise     = (Nothing, newGen)
      where
        as = filter ((== Alive) . aState) (aliens gstate)
        (i, gen1) = randomR (0, alienShootsOdds) gen
        (targetP, newGen) = pickPlayer gstate gen1
        -- every alien has 1 / alienBulletsOdds probability to shoot (max 1 per function call)
