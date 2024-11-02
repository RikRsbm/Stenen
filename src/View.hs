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
viewPure gstate@(GameState { status = PreStart }) -- add instructions
    = pictures [viewBasicStuff gstate, 
                case player2 gstate of
                Just p -> viewInstructions Singleplayer
                _      -> viewInstructions Multiplayer]
viewPure gstate = viewBasicStuff gstate -- otherwise, just draw: 1. the menu, or 2. all entities, the score and the status (if any)

viewBasicStuff :: GameState -> Picture
viewBasicStuff menu@(Menu {}) = pictures [mkPicture menu singleButton, mkPicture menu multiButton]
viewBasicStuff gstate@(GameState {}) = pictures pics
  where
    pics = steenPics ++ alienPics ++ bulletPics ++ alienBulletPics ++ [playerPic, player2Pic, scorePic, highscorePic, statusPic]

    playerPic = pict (player gstate)
    player2Pic = case player2 gstate of
                 Just p -> pict p
                 _      -> Blank

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
        c = pColor p

        lineBack  = color c (line [(x - pDx - pDx', y - pDy - pDy'),
                                   (x - pDx + pDx', y - pDy + pDy')])
        lineLeft  = color c (line [(x - pDx + pDx', y - pDy + pDy'),
                                   (x + pDx       , y + pDy       )])
        lineRight = color c (line [(x - pDx - pDx', y - pDy - pDy'),
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

    mkPicture gstate s@(Steen { sState = ExplosionState i _ }) 
        = translate x y (scale sc sc (steenAnimPics gstate !! fromEnum i))
      where 
        (x, y) = location s
        sc = 2 * radius s / implosionBmpSize

    mkPicture _ s = translate x y (color steenColor (circle (radius s))) -- it's still alive
      where (x, y) = location s

instance Viewable Alien where
    mkPicture :: GameState -> Alien -> Picture

    mkPicture gstate a@(Alien { aState = ExplosionState i _ }) 
        = translate x y (ufoAnimPics gstate !! fromEnum i)
      where 
        (x, y) = location a

    mkPicture gstate a = translate x y (ufoPic gstate) -- it's still alive
      where (x, y) = location a

instance Viewable Bullet where
    mkPicture :: GameState -> Bullet -> Picture
    mkPicture _ b = translate x y (color (bColor b) (circle bulletRadius))
      where 
        (x, y) = location b

instance Viewable Button where
    mkPicture :: GameState -> Button -> Picture
    mkPicture _ but = pictures [viewButton, viewButtonText]
      where
        viewButton = translate x y (color textColor (rectangleWire w h))
        viewButtonText = viewText (x - 140, y - 20) bigTextScale textColor (butText but)

        (x, y) = butLocation but 
        (w, h) = butSize but




viewScore :: Int -> Picture
viewScore s = viewText (250, 250) smallTextScale textColor ("Score: " ++ show s)

viewHighscore :: Int -> Picture
viewHighscore s = translate 190 210 (scale smallTextScale smallTextScale (color textColor (text ("Highscore: " ++ show s))))

viewStatus :: Status -> Picture
viewStatus PreStart = viewText (-180, statusY) bigTextScale textColor "Space to start"
viewStatus Paused = viewText (-300, statusY) bigTextScale textColor "Paused, Esc to resume"
viewStatus GameOver = viewText (-320, statusY) bigTextScale textColor "Game over, M for menu"
viewStatus _ = Blank

viewInstructions :: Mode -> Picture
viewInstructions Singleplayer = pictures [viewText (explanationX, 200) bigTextScale textColor "^ to boost"
                                , viewText (explanationX, 140) bigTextScale textColor "<, > to steer"
                                , viewText (explanationX, 80) bigTextScale textColor "Space to shoot"
                                , viewText (explanationX, 20) bigTextScale textColor "Esc to pause"]
viewInstructions _    = pictures [viewText (explanationX, 220) smallTextScale player1Color "^ to boost"
                                , viewText (explanationX, 180) smallTextScale player1Color "<, > to steer"
                                , viewText (explanationX, 140) smallTextScale player1Color "Space to shoot"
                                , viewText (explanationX, 80) smallTextScale player2Color "W to boost"
                                , viewText (explanationX, 40) smallTextScale player2Color "A, D to steer"
                                , viewText (explanationX, 0) smallTextScale player2Color "V to shoot"
                                , viewText (explanationX, -60) smallTextScale textColor "Esc to pause"]

viewText :: Point -> Float -> Color -> String -> Picture
viewText (x, y) s c txt = translate x y (scale s s (color c (text txt)))