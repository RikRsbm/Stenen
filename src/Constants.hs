module Constants where
    
import Graphics.Gloss
import General



lookDirectionVecMagnitude :: Float -- magnitude of lookDirection vector. leave this at 1. 
lookDirectionVecMagnitude = 1      -- This way we don't have to normalize the vector everytime we use it.
                                   -- If we change this, make sure that the vector gets normalized every time we use it

playerRadius :: Float -- distance between center and end of rocketship
playerRadius = 15

bulletRadius :: Float
bulletRadius = 4

playerBulletSpeed :: Float -- player bullet gets this speed + speed of player
playerBulletSpeed = 360

alienBulletSpeed :: Float -- alien bullet gets this speed
alienBulletSpeed = 240

steenScoreMultiplier :: Int
steenScoreMultiplier = 3

-- for editing steen values, edit them in the "perhapsCreateNew" function (too much variables to put here)

autoDecelPlayer :: Float -- amount of automatic decelleration per step
autoDecelPlayer = 2.5

inputAccelPlayer :: Float -- amount of acceleration with 'w' press per step
inputAccelPlayer = 9

inputSteerPlayer :: Float -- angle that player steers with 'a' or 'd' press per step
inputSteerPlayer = pi / 40

screenWidth :: Int
screenWidth = 1000

halfWidth :: Int
halfWidth = screenWidth `div` 2

halfWidthFloat :: Float
halfWidthFloat = fromIntegral halfWidth

screenHeight :: Int
screenHeight = 600

halfHeight :: Int
halfHeight = screenHeight `div` 2

halfHeightFloat :: Float
halfHeightFloat = fromIntegral halfHeight

highscorePath :: String
highscorePath = "highscore.txt"

bigTextScale :: Float
bigTextScale = 0.4

smallTextScale :: Float
smallTextScale = 0.25

statusY :: Float 
statusY = -150

explanationX :: Float 
explanationX = -450

playerBackLineRatio :: Float 
playerBackLineRatio = 0.5

alienRadius :: Float
alienRadius = 35

alienOdds :: Int
alienOdds = 400

alienBulletOdds :: Int
alienBulletOdds = 150

ufoScale :: Float
ufoScale = 2 * alienRadius / ufoBmpSize

ufoBmpSize :: Float
ufoBmpSize = 70

implosionBmpSize :: Float
implosionBmpSize = 70

alienSpeed :: Float
alienSpeed = 120

alienScoreMultiplier :: Int
alienScoreMultiplier = 4

bigUpdatesPerSec :: Float
bigUpdatesPerSec = 60

timePerSteenImplosionFrame :: Float
timePerSteenImplosionFrame = 0.1

timePerAlienImplosionFrame :: Float
timePerAlienImplosionFrame = 0.1

timePerBoostFrame :: Float
timePerBoostFrame = 0.05

boostBmpSize :: Float
boostBmpSize = 15

boostBmpScale :: Float
boostBmpScale = 0.7 * playerRadius / boostBmpSize




textColor :: Color
textColor = lightBlue

player1Color :: Color
player1Color = darkYellow

player2Color :: Color
player2Color = orange

alienColor :: Color
alienColor = pink

steenColor :: Color
steenColor = lightGray




pink :: Color
pink = makeColor 1 0.31 0.95 1

lightBlue :: Color
lightBlue = makeColor 0.5 0.5 1 1

darkYellow :: Color
darkYellow = makeColor 0.83 0.83 0 1

lightGray :: Color
lightGray = makeColor 0.9 0.9 0.9 1





singleButton :: Button
singleButton = Button (0, 100) buttonSize "Singleplayer"

multiButton :: Button
multiButton = Button (0, -100) buttonSize "Multiplayer"

buttonSize = (350, 100)


-- speeds are in pixels/sec