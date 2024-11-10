module Constants where
    

    
import Graphics.Gloss ( Color, orange, makeColor )





-- this module contains all constants that are used multiple times 
-- (including some that are only used once, for readability)




-- speeds, all speeds are in pixels/game tick

autoDecelPlayer :: Float -- amount of automatic decrease of player speed per game tick 
autoDecelPlayer = 0.04

inputAccelPlayer :: Float -- amount of added speed with 'w' press per game tick
inputAccelPlayer = 0.15

alienSpeed :: Float -- speed of alien
alienSpeed = 2

steenMinSpeed :: Float -- minimum possible speed of steen
steenMinSpeed = 0.5

steenMaxSpeed :: Float -- maximum possible speed of steen
steenMaxSpeed = 3

playerBulletSpeed :: Float -- speed of player bullet (relative to player velocity, so actual speed of bullet may vary) 
playerBulletSpeed = 8

alienBulletSpeed :: Float -- speed of alien bullet (relative to the point it was shot from, NOT relative to alien velocity)
alienBulletSpeed = 4





-- steering

inputSteerPlayer :: Float -- angle that player steers with 'a' or 'd' press per game tick, in radians
inputSteerPlayer = pi / 40






-- sizes

steenMinRadius :: Int -- minimum possible radius of steen
steenMinRadius = 15

steenMaxRadius :: Int -- maximum possible radius of steen
steenMaxRadius = 40

alienRadius :: Float -- radius of alien 
alienRadius = 35

playerRadius :: Float -- radius of player
playerRadius = 15

bulletRadius :: Float -- radius of bullet
bulletRadius = 4

playerBackLineRatio :: Float -- back line size / player diameter 
playerBackLineRatio = 0.5







-- randomisation, per game tick there is a 1 in (odds) chance of the event happening

steenCreationOdds :: Int
steenCreationOdds = 65

alienCreationOdds :: Int
alienCreationOdds = 350

alienShootsOdds :: Int
alienShootsOdds = 150







-- kill rewards, these values gets added to the score when you shoot a steen / alien

steenScoreMultiplier :: Int
steenScoreMultiplier = 3

alienScoreMultiplier :: Int
alienScoreMultiplier = 5






-- colors

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






-- selfmade colors

pink :: Color
pink = makeColor 1 0.31 0.95 1

lightBlue :: Color
lightBlue = makeColor 0.5 0.5 1 1

darkYellow :: Color
darkYellow = makeColor 0.83 0.83 0 1

lightGray :: Color
lightGray = makeColor 0.9 0.9 0.9 1






-- text size and positioning

bigTextScale :: Float -- compared to standard text size
bigTextScale = 0.4

smallTextScale :: Float -- compared to standard text size
smallTextScale = 0.25

statusY :: Float  -- y-value that status information is displayed at
statusY = -150

instructionX :: Float -- x-value that instructions get displayed at
instructionX = -450







-- animations

ticksPerImplosionFrame :: Float -- amount of ticks that each implosion frame takes
ticksPerImplosionFrame = 6

ticksPerBoostFrame :: Float -- amount of ticks that each boost frame takes
ticksPerBoostFrame = 3

boostBmpSize :: Float -- the height (short side) (in pixels) of the boost pictures in the Pictures folder
boostBmpSize = 15

alienBmpSize :: Float -- the width (in pixels) of the alien (and alien implosion) pictures in the Pictures folder
alienBmpSize = 70

steenImplosionBmpSize :: Float -- the width (in pixels) of the steen implosion pictures in the Pictures folder
steenImplosionBmpSize = 70

alienPicsScale :: Float -- when scaled by this constant, the alien pictures get the proper size, corresponding with alienRadius
alienPicsScale = 2 * alienRadius / alienBmpSize

boostPicsScale :: Float -- when scaled by this constant, the boost pictures get the proper size, corresponding with the size of the back line of the player
boostPicsScale = 0.7 * (playerBackLineRatio * 2 * playerRadius) / boostBmpSize






-- miscellaneous

buttonSize :: (Float, Float) -- size of the menu buttons
buttonSize = (350, 100)

gameTicksPerSec :: Float -- how often the game updates its state. Increasing this will speed up the game
gameTicksPerSec = 60

highscorePath :: String -- location of file with highscores
highscorePath = "highscore.txt"

screenWidth :: Int -- width of screen
screenWidth = 1000

screenHeight :: Int -- height of screen
screenHeight = 600

widthHeightRatio :: Float
widthHeightRatio = fromIntegral screenWidth / fromIntegral screenHeight

picturesFolder :: String -- location of folder where the pictures are located
picturesFolder = "Pictures/"