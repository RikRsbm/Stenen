{-# LANGUAGE InstanceSigs #-}
module View where

import Model
import General
import Constants
import Graphics.Gloss.Data.Vector 
import Graphics.Gloss
import Data.Fixed




view :: GameState -> IO Picture
view gstate = return (viewPure gstate)

viewPure :: GameState -> Picture
viewPure gstate = pictures pics
  where
    pics = steenPics ++ alienPics ++ bulletPics ++ alienBulletPics ++ [playerPic, scorePic, highscorePic, statusPic]

    playerPic = pict (player gstate)
    steenPics = map pict (stenen gstate)
    alienPics = map pict (aliens gstate)
    bulletPics = map pict (bullets gstate)
    alienBulletPics = map pict (alienBullets gstate)
    
    scorePic = viewScore (score gstate)
    highscorePic = viewHighscore (highscore gstate)
    statusPic = viewStatus (status gstate)

    pict :: Viewable a => a -> Picture
    pict = mkPicture gstate






class Viewable a where
    mkPicture :: GameState -> a -> Picture

instance Viewable Player where
    mkPicture :: GameState -> Player -> Picture
    mkPicture gstate p = pictures [lineLeft, lineRight, lineBack, boostPic]
      where 
        (x, y) = location p
        d@(dx, dy) = lookDirection p
        (dx', dy') = rotateV (pi / 2) d

        lineBack  = color playerColor (line [(x - pDx - pDx', y - pDy - pDy'),
                                      (x - pDx + pDx', y - pDy + pDy')])
        lineLeft  = color playerColor (line [(x - pDx + pDx', y - pDy + pDy'),
                                      (x + pDx       , y + pDy       )])
        lineRight = color playerColor (line [(x - pDx - pDx', y - pDy - pDy'),
                                      (x + pDx       , y + pDy       )])
        pDx = playerRadius * dx
        pDy = playerRadius * dy
        pDx' = playerRadius * dx' * playerBackLineRatio
        pDy' = playerRadius * dy' * playerBackLineRatio

        boostPic = mkBoostPicture gstate p (pDx, pDy)

mkBoostPicture :: GameState -> Player -> Point -> Picture
mkBoostPicture gstate p@(Player { boostState = BoostFrame i _}) (pDx, pDy)
    = translate (x - pDx - boostBmpScale * pDx) 
                (y - pDy - boostBmpScale * pDy) 
                (rotate angle (boostAnimPics gstate !! fromEnum i))
  where 
    (x, y) = location p
    angle = 90 + radToDeg (atan2 pDx pDy)
    radToDeg rad = rad * (180 / pi)
mkBoostPicture _ _ _ = Blank


instance Viewable Steen where
    mkPicture :: GameState -> Steen -> Picture

    mkPicture  gstate s@(Steen { sState = ExplosionState i _ }) 
        = translate x y (scale sc sc (steenAnimPics gstate !! fromEnum i))
      where 
        (x, y) = location s
        sc = 2 * radius s / implosionBmpSize

    mkPicture _ s = translate x y (color lightGray (circle (radius s))) -- it's still alive
      where (x, y) = location s

instance Viewable Alien where
    mkPicture :: GameState -> Alien -> Picture
    mkPicture gstate a = translate x y (ufoPic gstate)
      where (x, y) = location a

instance Viewable Bullet where
    mkPicture :: GameState -> Bullet -> Picture
    mkPicture _ b = translate x y (color c (circle bulletRadius))
      where 
        (x, y) = location b
        c | bColor b == Yellow = playerColor
          | otherwise          = pink






viewScore :: Int -> Picture
viewScore s = viewText (250, 250) smallTextScale textColor ("Score: " ++ show s)

viewHighscore :: Int -> Picture
viewHighscore s = translate 190 210 (scale smallTextScale smallTextScale (color textColor (text ("Highscore: " ++ show s))))

viewStatus :: Status -> Picture
viewStatus PreStart = pictures [viewText (explanationX, 200) bigTextScale textColor "W to boost"
                              , viewText (explanationX, 140) bigTextScale textColor "A, D to steer"
                              , viewText (explanationX, 80) bigTextScale textColor "Enter to shoot"
                              , viewText (explanationX, 20) bigTextScale textColor "Esc to pause"
                              , viewText (-150, statusY) bigTextScale textColor "W to start"]
viewStatus Paused = viewText (-300, statusY) bigTextScale textColor "paused, Esc to resume"
viewStatus GameOver = viewText (-320, statusY) bigTextScale textColor "Game over, R to restart"
viewStatus _ = Blank

viewText :: Point -> Float -> Color -> String -> Picture
viewText (x, y) s c txt = translate x y (scale s s (color c (text txt)))