module Constants where
    
import Graphics.Gloss



lookDirectionVecMagnitude :: Float -- magnitude of lookDirection vector. leave this at 1. 
lookDirectionVecMagnitude = 1      -- This way we don't have to normalize the vector everytime we use it.
                                   -- If we change this, make sure that the vector gets normalized every time we use it

playerRadius :: Float -- distance between center and end of rocketship
playerRadius = 15

bulletRadius :: Float
bulletRadius = 4

playerBulletSpeed :: Float -- player bullet gets this speed + speed of player
playerBulletSpeed = 6

alienBulletSpeed :: Float -- alien bullet gets this speed
alienBulletSpeed = 4

steenScoreMultiplier :: Int
steenScoreMultiplier = 3

-- for editing steen values, edit them in the "perhapsCreateNew" function (too much variables to put here)

autoDecelPlayer :: Float -- amount of automatic decelleration per step
autoDecelPlayer = 0.04

inputAccelPlayer :: Float -- amount of acceleration per 'w' press
inputAccelPlayer = 0.15

inputSteerPlayer :: Float -- angle that player steers per 'a' or 'd' press
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
alienBulletOdds = 100

lightPink :: Color
lightPink = makeColor 1.0 0.7 0.8 1.0

ufoScale :: Float
ufoScale = 2 * alienRadius / ufoBmpSize

ufoBmpSize :: Float
ufoBmpSize = 70

alienSpeed :: Float
alienSpeed = 2

alienScoreMultiplier :: Int
alienScoreMultiplier = 4