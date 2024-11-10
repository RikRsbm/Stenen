{-# LANGUAGE InstanceSigs #-}
module RandomObjectClass where




import TempObjectClass ( TempObject )
import DataTypes
    ( DieState(Alive),
      Bullet,
      Alien(Alien, aState),
      Steen(Steen),
      GameState(aliens) )
import System.Random ( StdGen, Random(randomR) )
import Constants
    ( alienSpeed,
      steenMinSpeed,
      steenMaxSpeed,
      steenMinRadius,
      steenMaxRadius,
      alienRadius,
      steenCreationOdds,
      alienCreationOdds,
      alienShootsOdds,
      screenWidth,
      screenHeight )
import Graphics.Gloss.Data.Vector ( mulSV, normalizeV )
import MovableClass ( Movable(location) )
import CanShootClass ( CanShoot(shootBullet) )
import MorePlayerLogic ( pickPlayer )




-- this class generalizes the objects that are randomly created



class (TempObject a) => RandomObject a where
    perhapsCreateNew :: GameState -> StdGen -> (Maybe a, StdGen) -- might create a new object (small chance). also updates the generator  
    

instance RandomObject Steen where
    perhapsCreateNew :: GameState -> StdGen -> (Maybe Steen, StdGen)
    perhapsCreateNew gstate gen 
        | creationOdds == 0 = (Just (Steen (x, y) (dx, dy) r Alive), newGen) -- a new steen gets created
        | otherwise         = (Nothing, gen1)                                -- no new creation
      where
        (creationOdds, gen1  ) = randomR (0                    , steenCreationOdds  ) gen  -- if this value is 0, a new steen gets created
        (radius      , gen2  ) = randomR (steenMinRadius       , steenMaxRadius     ) gen1 -- radius of steen
        (randomX     , gen3  ) = randomR (- halfWidth  - radius, halfWidth  + radius) gen2 -- x-value that steen spawns at (for if it spawns at the top or bottom side). - resp + radius so that the steen spawns outside the screen
        (randomY     , gen4  ) = randomR (- halfHeight - radius, halfHeight + radius) gen3 -- same but for y-value
        (speed       , gen5  ) = randomR (steenMinSpeed        , steenMaxSpeed      ) gen4 -- speed of steen
        (side        , gen6  ) = randomR (0  :: Int            , 3                  ) gen5 -- side of screen the steen spawns at
        (targetP     , newGen) = pickPlayer gstate gen6 -- pick a player, steen will target this player

        (x, y) = case side of 
                  0 -> (fromIntegral randomX, - halfHeightFloat - r) -- bottom side
                  1 -> (fromIntegral randomX,   halfHeightFloat + r) -- top side
                  2 -> (- halfWidthFloat - r, fromIntegral randomY) -- left side
                  _ -> (  halfWidthFloat + r, fromIntegral randomY) -- right side

        (dx, dy) = speed `mulSV` normalizeV (a - x, b - y) -- velocity of steen
        (a, b) = location targetP 
        r = fromIntegral radius

        halfWidth = screenWidth `div` 2
        halfHeight = screenHeight `div` 2
        halfWidthFloat = fromIntegral halfWidth
        halfHeightFloat = fromIntegral halfHeight


instance RandomObject Alien where 
    perhapsCreateNew :: GameState -> StdGen -> (Maybe Alien, StdGen)
    perhapsCreateNew _ gen 
        | creationOdds == 0 = (Just (Alien (x, y) (dx, dy) Alive), newGen) -- a new alien gets created
        | otherwise         = (Nothing, gen1)                              -- no new creation
      where
        (creationOdds, gen1  ) = randomR (0               , alienCreationOdds) gen  -- if this value is 0, an alien gets created
        (randomX     , gen2  ) = randomR (- halfWidth  + r, halfWidth  - r   ) gen1 -- x-value that alien spawns at (for if it spawns at the top or bottom side). + resp - r ensures that the alien doesn't stay outside of the screen
        (randomY     , gen3  ) = randomR (- halfHeight + r, halfHeight - r   ) gen2 -- same but for y-value
        (side        , newGen) = randomR (0  :: Int       , 3                ) gen3 -- side of the screen the alien spawns at

        (x, y, dx, dy) = case side of 
                          0 -> (fromIntegral randomX          , - halfHeightFloat - alienRadius, 0           ,   alienSpeed) -- bottom side
                          1 -> (fromIntegral randomX          ,   halfHeightFloat + alienRadius, 0           , - alienSpeed) -- top side
                          2 -> (- halfWidthFloat - alienRadius, fromIntegral randomY           ,   alienSpeed, 0           ) -- left side
                          _ -> (  halfWidthFloat + alienRadius, fromIntegral randomY           , - alienSpeed, 0           ) -- right side

        r = round alienRadius 

        halfWidth = screenWidth `div` 2
        halfHeight = screenHeight `div` 2
        halfWidthFloat = fromIntegral halfWidth
        halfHeightFloat = fromIntegral halfHeight

instance RandomObject Bullet where
    perhapsCreateNew :: GameState -> StdGen -> (Maybe Bullet, StdGen) 
    perhapsCreateNew gstate gen 
        | i < length as = (Just (shootBullet (as!!i) targetP), newGen) -- if the random number is within the alive alien list length, that alien shoots a bullet towards the target player
        | otherwise     = (Nothing, newGen)                            -- otherwise, no new creation
      where
        as = filter ((== Alive) . aState) (aliens gstate) -- the alive aliens
        (i, gen1) = randomR (0, alienShootsOdds) gen      -- picks a number between 0 and alienShootOdds
        (targetP, newGen) = pickPlayer gstate gen1        -- picks the target player
    -- in other words, every alien has 1 / alienBulletsOdds probability to shoot (max 1 per function call)
