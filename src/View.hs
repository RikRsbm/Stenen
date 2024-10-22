module View where

import Model
import General
import Constants
import Graphics.Gloss.Data.Vector 
import Graphics.Gloss




view :: GameState -> IO Picture
view gstate = return (viewPure gstate)

viewPure :: GameState -> Picture
viewPure gstate = pictures pics
  where
    pics = steenPics ++ alienPics ++ bulletPics ++ alienBulletPics ++ [playerPic, scorePic, highscorePic, statusPic]

    playerPic = viewPlayer (player gstate)
    steenPics = map (viewSteen (implosionPics gstate)) (stenen gstate)
    alienPics = map (viewAlien (ufoPic gstate)) (aliens gstate)
    bulletPics = map (viewBullet red) (bullets gstate)
    alienBulletPics = map (viewBullet lightPink) (alienBullets gstate)
    scorePic = viewScore (score gstate)
    highscorePic = viewHighscore (highscore gstate)
    statusPic = viewStatus (status gstate)



viewPlayer :: Player -> Picture
viewPlayer p = pictures [lineLeft, lineRight, lineBack]
  where 
    (x, y) = location p
    d@(dx, dy) = lookDirection p
    (dx', dy') = rotateV (pi / 2) d

    lineBack  = color green (line [(x - pDx - pDx', y - pDy - pDy'),
                                   (x - pDx + pDx', y - pDy + pDy')])
    lineLeft  = color green (line [(x - pDx + pDx', y - pDy + pDy'),
                                   (x + pDx       , y + pDy       )])
    lineRight = color green (line [(x - pDx - pDx', y - pDy - pDy'),
                                   (x + pDx       , y + pDy       )])
    pDx = playerRadius * dx
    pDy = playerRadius * dy
    pDx' = playerRadius * dx' * playerBackLineRatio
    pDy' = playerRadius * dy' * playerBackLineRatio






viewSteen :: [Picture] -> Steen -> Picture
viewSteen _ s@(Steen { animationState = NotExploded }) = viewRoundSteen s
viewSteen pics s@(Steen { animationState = AnimationState i _ }) = viewPictureSteen (pics !! fromEnum i) s

viewPictureSteen :: Picture -> Steen -> Picture
viewPictureSteen pic s = translate x y (scale sc sc pic)
  where 
    (x, y) = location s
    sc = 2 * radius s / implosionBmpSize

viewRoundSteen :: Steen -> Picture
viewRoundSteen s = translate x y (color white (circle (radius s))) 
  where (x, y) = location s





viewAlien :: Picture -> Alien -> Picture
viewAlien ufo a = translate x y ufo
  where (x, y) = location a

viewBullet :: Color -> Bullet -> Picture
viewBullet c b = translate x y (color c (circle bulletRadius))
  where (x, y) = location b

viewScore :: Int -> Picture
viewScore s = viewText (250, 250) smallTextScale blue ("Score: " ++ show s)

viewHighscore :: Int -> Picture
viewHighscore s = translate 190 210 (scale smallTextScale smallTextScale (color blue (text ("Highscore: " ++ show s))))

viewStatus :: Status -> Picture
viewStatus PreStart = pictures [viewText (explanationX, 200) bigTextScale blue "W to boost"
                              , viewText (explanationX, 140) bigTextScale blue "A, D to steer"
                              , viewText (explanationX, 80) bigTextScale blue "Enter to shoot"
                              , viewText (explanationX, 20) bigTextScale blue "Esc to pause"
                              , viewText (-150, statusY) bigTextScale blue "W to start"]
viewStatus Paused = viewText (-300, statusY) bigTextScale blue "paused, Esc to resume"
viewStatus GameOver = viewText (-160, statusY) bigTextScale blue "R to restart"
viewStatus _ = Blank

viewText :: Point -> Float -> Color -> String -> Picture
viewText (x, y) s c txt = translate x y (scale s s (color c (text txt)))

