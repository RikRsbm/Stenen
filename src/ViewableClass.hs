{-# LANGUAGE InstanceSigs #-}
module ViewableClass where



import DataTypes
import Graphics.Gloss
import MovableClass
import Graphics.Gloss.Data.Vector
import Constants
import General






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
    = translate (x - pDx - boostPicsScale * pDx) 
                (y - pDy - boostPicsScale * pDy) 
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
        sc = 2 * radius s / steenImplosionBmpSize

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
