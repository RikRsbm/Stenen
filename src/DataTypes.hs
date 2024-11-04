module DataTypes where



import Constants
import Graphics.Gloss 
import System.Random 




data GameState = GameState { 
                   player :: Player
                 , player2 :: Maybe Player 
                 , stenen :: [Steen] -- list of onscreen asteroids
                 , bullets :: [Bullet] -- list of onscreen bullets
                 , aliens :: [Alien] -- list of onscreen aliens
                 , alienBullets :: [Bullet] -- list of onscreen bullets from the alien
                 , status :: GameStatus -- status of game
                 , score :: Int -- current score
                 , highscore :: Int -- all time highscore (gets loaded in at start of game)
                 , elapsedTime :: Float
                 , gen :: StdGen
                 , ufoPic :: Picture
                 , steenAnimPics :: [Picture]
                 , ufoAnimPics :: [Picture]
                 , boostAnimPics :: [Picture]
                 }
                 | 
                 Menu {             
                   ufoPic :: Picture
                 , steenAnimPics :: [Picture]
                 , ufoAnimPics :: [Picture]
                 , boostAnimPics :: [Picture]
                 }

initialState :: Picture -> [Picture] -> [Picture] -> [Picture] -> GameState
initialState = GameState initialPlayer1
                         Nothing
                         []
                         []
                         []
                         []
                         FirstStep
                         0
                         0
                         0
                         (mkStdGen 0) -- will be changed to a random seed in the first step





data Player = Player { 
                pLocation :: Point -- location of player
              , pVelocity :: Vector -- velocity of player
              , lookDirection :: Vector -- direction that player is looking in, it's a vector of constant magnitude 1. That way we don't have to normalize it if we want to use it
              , boostState :: BoostState -- is the player boosting? if so, which boost frame, and how long has it been shown?
              , pColor :: Color
              , forwardPressed :: Bool
              , leftPressed :: Bool
              , rightPressed :: Bool
              } 
              deriving Eq

initialPlayer1 :: Player
initialPlayer1 = Player (0, 0) 
                 (0, 0) 
                 (0, 1)
                 NotBoosting
                 player1Color
                 False
                 False
                 False

initialPlayer2 :: Player
initialPlayer2 = initialPlayer1 { pColor = player2Color, pLocation = (0, -50) }




data Steen = Steen { 
               sLocation :: Point -- location of asteroid
             , sVelocity :: Vector -- velocity of asteroid
             , sRadius :: Float -- radius of asteroid
             , sState :: DieState -- state of the animation
             } 

data Alien = Alien {
               aLocation :: Point -- location of alien
             , aVelocity :: Vector -- velocity of alien
             , aState :: DieState -- state of the animation
             }

data Bullet = Bullet {
                bLocation :: Point -- location of bullet
              , bVelocity :: Vector -- velocity of bullet
              , bColor :: Color -- color of Bullet
              }






data GameStatus = FirstStep -- the first step of the game, it then reads the highscore from highscore.txt
                | PreStart -- between FirstStep and the first 'w' press
                | Running -- while the game is running
                | Paused -- while the game is paused
                | GameOver -- when the game is over
                deriving Eq

data DieState = Alive 
           | Dying ZeroToFour Float -- which frame, how many seconds has it been at this frame
           | Dead
           deriving (Show, Eq)

data BoostState = NotBoosting
                | BoostFrame ZeroToTwo Float -- which frame, how many seconds it has been at this frame
                deriving (Show, Eq)

data ZeroToFour = Zero4 | One4 | Two4 | Three4 | Four4
                  deriving (Show, Eq, Enum, Bounded)

data ZeroToTwo = Zero2 | One2 | Two2
                 deriving (Show, Eq, Enum, Bounded)

data BulletType = FromPlayer | FromAlien deriving (Show, Eq)

data Direction = Forward | Left | Right

data Mode = Singleplayer | Multiplayer




data Button = Button {
                butLocation :: Point
              , butSize :: Point
              , butText :: String
              }
              deriving Eq

singleButton :: Button
singleButton = Button (0, 100) buttonSize "Singleplayer"

multiButton :: Button
multiButton = Button (0, -100) buttonSize "Multiplayer"