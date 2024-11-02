module Constants where
    

    
import Graphics.Gloss





-- speeds, all speeds are in pixels/sec

autoDecelPlayer :: Float -- amount of automatic decrease of speed per step
autoDecelPlayer = 2.5

inputAccelPlayer :: Float -- amount of added speed with 'w' press per step
inputAccelPlayer = 9

alienSpeed :: Float
alienSpeed = 120

steenMinSpeed :: Int
steenMinSpeed = 60

steenMaxSpeed :: Int
steenMaxSpeed = 120

playerBulletSpeed :: Float -- player bullet gets this speed + speed of player
playerBulletSpeed = 360

alienBulletSpeed :: Float -- alien bullet gets this speed
alienBulletSpeed = 240





-- steering

inputSteerPlayer :: Float -- angle that player steers with 'a' or 'd' press per step, in radians
inputSteerPlayer = pi / 40






-- sizes

steenMinRadius :: Int
steenMinRadius = 15

steenMaxRadius :: Int
steenMaxRadius = 40

alienRadius :: Float
alienRadius = 35

playerRadius :: Float 
playerRadius = 15

bulletRadius :: Float
bulletRadius = 4

playerBackLineRatio :: Float 
playerBackLineRatio = 0.5







-- randomisation, per step there is a 1 in (odds) chance of a spawn

steenCreationOdds :: Int
steenCreationOdds = 100

alienCreationOdds :: Int
alienCreationOdds = 400

alienShootsOdds :: Int
alienShootsOdds = 150







-- score rewards

steenScoreMultiplier :: Int
steenScoreMultiplier = 3

alienScoreMultiplier :: Int
alienScoreMultiplier = 4






-- assigned colors

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






-- colors

pink :: Color
pink = makeColor 1 0.31 0.95 1

lightBlue :: Color
lightBlue = makeColor 0.5 0.5 1 1

darkYellow :: Color
darkYellow = makeColor 0.83 0.83 0 1

lightGray :: Color
lightGray = makeColor 0.9 0.9 0.9 1






-- text size and positioning

bigTextScale :: Float
bigTextScale = 0.4

smallTextScale :: Float
smallTextScale = 0.25

statusY :: Float 
statusY = -150

explanationX :: Float 
explanationX = -450







-- animations

timePerSteenImplosionFrame :: Float
timePerSteenImplosionFrame = 0.1

timePerAlienImplosionFrame :: Float
timePerAlienImplosionFrame = 0.1

timePerBoostFrame :: Float
timePerBoostFrame = 0.05

boostBmpSize :: Float
boostBmpSize = 15

ufoBmpSize :: Float
ufoBmpSize = 70

steenImplosionBmpSize :: Float
steenImplosionBmpSize = 70

boostPicsScale :: Float -- first number is scale of boost animation compared to size of player
boostPicsScale = 0.7 * playerRadius / boostBmpSize






-- miscellaneous

buttonSize :: (Float, Float)
buttonSize = (350, 100)

bigUpdatesPerSec :: Float
bigUpdatesPerSec = 60

highscorePath :: String
highscorePath = "highscore.txt"

screenWidth :: Int
screenWidth = 1000

screenHeight :: Int
screenHeight = 600