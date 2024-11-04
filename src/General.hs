module General where


import Graphics.Gloss 
import Graphics.Gloss.Data.Vector 
import DataTypes







withinButtonBounds :: Point -> Button -> Bool
withinButtonBounds (x,y) but = x > x' - halfW && x < x' + halfW
                            && y > y' - halfH && y < y' + halfH
  where 
    (x', y') = butLocation but
    (halfW, halfH) = 0.5 `mulSV` butSize but 





addVec :: Vector -> Vector -> Vector
addVec (dx, dy) (dx', dy') = (dx + dx',
                              dy + dy')

subVec :: Vector -> Vector -> Vector
subVec (dx, dy) (dx', dy') = (dx - dx',
                              dy - dy')
                        
addVecToPt :: Point -> Vector -> Point
addVecToPt (x, y) (x', y') = (x + x', 
                              y + y')

pointsWithinDistance :: Point -> Point -> Float -> Bool
pointsWithinDistance (x, y) (a, b) d = magV (x - a, y - b) < d





viewText :: Point -> Float -> Color -> String -> Picture
viewText (x, y) s c txt = translate x y (scale s s (color c (text txt)))