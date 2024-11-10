{-# LANGUAGE InstanceSigs #-}
module ViewableClass where



import DataTypes
    ( Button(butSize, butText, butLocation),
      BoostState(BoostFrame),
      DieState(Dying),
      Bullet(bColor),
      Alien(Alien, aState),
      Steen(Steen, sState),
      Player(Player, lookDirection, pColor, boostState),
      GameState(alienAnimPics, alienPic, steenAnimPics, boostAnimPics) )
import Graphics.Gloss
    ( Point,
      Picture(Blank),
      translate,
      scale,
      color,
      circle,
      line,
      pictures,
      rectangleWire,
      rotate )
import MovableClass ( Movable(location, radius) )
import Graphics.Gloss.Data.Vector ( rotateV )
import Constants
    ( playerRadius,
      bulletRadius,
      playerBackLineRatio,
      textColor,
      steenColor,
      bigTextScale,
      steenImplosionBmpSize,
      boostPicsScale )
import General ( viewText )




-- this module generalizes all datatypes that can be turned into a picture




class Viewable a where
    mkPicture :: GameState -> a -> Picture -- turn the object into a picture


instance Viewable Player where
    mkPicture :: GameState -> Player -> Picture -- draw the player in the lookDirection
    mkPicture gstate p = pictures [lineLeft, lineRight, lineBack, boostPic]
      where 
        (x, y) = location p
        d@(dx, dy) = lookDirection p
        (dx', dy') = rotateV (pi / 2) d
        c = pColor p

        lineBack  = color c (line [(x - px - px', y - py - py'), -- back line
                                   (x - px + px', y - py + py')])
        lineLeft  = color c (line [(x - px + px', y - py + py'), -- left line
                                   (x + px      , y + py      )])
        lineRight = color c (line [(x - px - px', y - py - py'), -- right line
                                   (x + px      , y + py      )])
        px = playerRadius * dx
        py = playerRadius * dy
        px' = playerRadius * dx' * playerBackLineRatio
        py' = playerRadius * dy' * playerBackLineRatio

        boostPic = mkBoostPicture gstate p (px, py) -- make the boost picture

mkBoostPicture :: GameState -> Player -> Point -> Picture -- make the boost picture
mkBoostPicture gstate p@(Player { boostState = BoostFrame i _}) (px, py) -- if the player is boosting, paste the boost picture at the back of the player
    = translate (x - px - boostPicsScale * px) 
                (y - py - boostPicsScale * py) 
                (rotate angle (boostAnimPics gstate !! fromEnum i)) -- find the correct picture and rotate it the correct amount of degrees
  where 
    (x, y) = location p
    angle = 90 + radToDeg (atan2 px py)
    radToDeg rad = rad * (180 / pi)
mkBoostPicture _ _ _ = Blank -- it the player is not boosting, don't draw anything


instance Viewable Steen where
    mkPicture :: GameState -> Steen -> Picture

    mkPicture gstate s@(Steen { sState = Dying i _ }) -- if the steen is dying, draw the correct implosion animation frame
        = translate x y (scale sc sc (steenAnimPics gstate !! fromEnum i))
      where 
        (x, y) = location s
        sc = 2 * radius s / steenImplosionBmpSize

    mkPicture _ s = translate x y (color steenColor (circle (radius s))) -- it's still alive, draw a normal steen
      where (x, y) = location s


instance Viewable Alien where
    mkPicture :: GameState -> Alien -> Picture

    mkPicture gstate a@(Alien { aState = Dying i _ }) -- if the alien is dying, draw the correct implosion animation frame
        = translate x y (alienAnimPics gstate !! fromEnum i)
      where 
        (x, y) = location a

    mkPicture gstate a = translate x y (alienPic gstate) -- it's still alive, draw a normal alien
      where (x, y) = location a


instance Viewable Bullet where
    mkPicture :: GameState -> Bullet -> Picture
    mkPicture _ b = translate x y (color (bColor b) (circle bulletRadius)) -- draw the bullet
      where 
        (x, y) = location b


instance Viewable Button where
    mkPicture :: GameState -> Button -> Picture
    mkPicture _ but = pictures [viewButton, viewButtonText]
      where
        viewButton = translate x y (color textColor (rectangleWire w h)) -- draw a rectangle where the button is
        viewButtonText = viewText (x - 140, y - 20) bigTextScale textColor (butText but) -- draw the button text

        (x, y) = butLocation but 
        (w, h) = butSize but