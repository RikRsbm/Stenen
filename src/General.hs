module General where


import Graphics.Gloss
    ( Point, Color, Picture, Vector, translate, scale, color, text )
import Graphics.Gloss.Data.Vector ( mulSV, magV )
import DataTypes ( Button(butSize, butLocation) )




-- this module contains general-purpose funtions (could be used in other projects)




withinButtonBounds :: Point -> Button -> Bool -- is the point within the bounds of the button?
withinButtonBounds (x,y) but = x > x' - halfW && x < x' + halfW
                            && y > y' - halfH && y < y' + halfH
  where
    (x', y') = butLocation but
    (halfW, halfH) = 0.5 `mulSV` butSize but





addVec :: Vector -> Vector -> Vector -- adds two vectors (can also be used with points)
addVec (dx, dy) (dx', dy') = (dx + dx',
                              dy + dy')

subVec :: Vector -> Vector -> Vector -- subtracts a vector from another (can also be used with points)
subVec (dx, dy) (dx', dy') = (dx - dx',
                              dy - dy')

pointsWithinDistance :: Point -> Point -> Float -> Bool -- are the two points within the provided distance of each other?
pointsWithinDistance (x, y) (a, b) d = magV (x - a, y - b) < d





viewText :: Point -> Float -> Color -> String -> Picture -- write provided text at provided point, with provided scale and color
viewText (x, y) s c txt = translate x y (scale s s (color c (text txt)))