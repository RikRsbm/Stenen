module DataTypes where



import Constants
import Graphics.Gloss 
import System.Random 




-- this module contains all data types and some basic values for some of those types




data GameState = GameState { 
                   player :: Player -- player 1
                 , player2 :: Maybe Player -- player 2 ('Nothing' in case of singleplayer)
                 , stenen :: [Steen] -- list of onscreen asteroids
                 , bullets :: [Bullet] -- list of onscreen bullets
                 , aliens :: [Alien] -- list of onscreen aliens
                 , alienBullets :: [Bullet] -- list of onscreen bullets from the aliens
                 , status :: GameStatus -- status of game
                 , score :: Int -- current score
                 , highscore :: Int -- all time highscore (gets loaded in at start of game)
                 , elapsedTime :: Float -- to keep track of time (so that we know when to do a game tick)
                 , gen :: StdGen -- to create random numbers (gets updated after every use)
                 , alienPic :: Picture -- alien picture
                 , steenAnimPics :: [Picture] -- pictures for steen implosion animation
                 , alienAnimPics :: [Picture] -- pictures for alien implosion animation
                 , boostAnimPics :: [Picture] -- pictures for boost animation
                 }
                 | 
                 Menu {             
                   alienPic :: Picture -- see above
                 , steenAnimPics :: [Picture] -- see above
                 , alienAnimPics :: [Picture] -- see above
                 , boostAnimPics :: [Picture] -- see above
                 }

initialSingleplayerState :: Picture -> [Picture] -> [Picture] -> [Picture] -> GameState -- initial state for singleplayer
initialSingleplayerState = GameState initialPlayer1
                                     Nothing
                                     []
                                     []
                                     []
                                     []
                                     FirstStep
                                     0
                                     0
                                     0
                                     (mkStdGen 0) -- will be changed to a random seed (by using IO) in the first step





data Player = Player { 
                pLocation :: Point -- location of player
              , pVelocity :: Vector -- velocity of player
              , lookDirection :: Vector -- direction that player is looking in
              , boostState :: BoostState -- boost state of player
              , pColor :: Color -- color of player and his bullets
              , leftPressed :: Bool -- does the player press the button that is bound to 'steer left'?
              , rightPressed :: Bool -- does the player press the button that is bound to 'steer right'?
              } 
              deriving Eq

initialPlayer1 :: Player -- initial state for player 1
initialPlayer1 = Player 
                 (0, 0) 
                 (0, 0) 
                 (0, 1) -- lookDirection is a vector of constant magnitude 1. That way we don't have to normalize it every time we want to use it
                 NotBoosting
                 player1Color
                 False
                 False

initialPlayer2 :: Player -- initial state for player 2
initialPlayer2 = initialPlayer1 { pColor = player2Color, pLocation = (0, -50) }




data Steen = Steen { -- steen is our word for asteroid (since our game is called stenen)
               sLocation :: Point -- location of asteroid
             , sVelocity :: Vector -- velocity of asteroid
             , sRadius :: Float -- radius of asteroid
             , sState :: DieState -- state of the implosion animation
             } 

data Alien = Alien {
               aLocation :: Point -- location of alien
             , aVelocity :: Vector -- velocity of alien
             , aState :: DieState -- state of the implosion animation
             }

data Bullet = Bullet {
                bLocation :: Point -- location of bullet
              , bVelocity :: Vector -- velocity of bullet
              , bColor :: Color -- color of Bullet
              }






data GameStatus = FirstStep -- the first step of the game, it then reads the highscore from highscore.txt
                | PreStart -- between FirstStep and the actual start of the game
                | Running -- while the game is running
                | Paused -- while the game is paused
                | GameOver -- when the game is over
                deriving Eq

data DieState = Alive -- object is not (yet) shot down
              | Dying ZeroToFour Float -- which implosion animation frame, how many seconds has it been at this frame
              | Dead -- implosion animation has finished
              deriving (Show, Eq)

data BoostState = NotBoosting -- player is not pressing the key bound to boosting
                | BoostFrame ZeroToTwo Float -- player is pressing the key bound to boosting. Which boost animation frame 
                                             -- is currently being displayed, how many seconds has it been at this frame
                deriving (Show, Eq)

data ZeroToFour = Zero4 | One4 | Two4 | Three4 | Four4 -- datatype for the integers 0, 1, 2, 3 and 4
                  deriving (Show, Eq, Enum, Bounded)

data ZeroToTwo = Zero2 | One2 | Two2 -- datatype for the integers 0, 1 and 2
                 deriving (Show, Eq, Enum, Bounded)

data MovementKeys = Boost | SteerLeft | SteerRight -- datatype for the three movement keys

data Mode = Singleplayer | Multiplayer -- singleplayer vs multiplayer, used in the pattern matching for viewInstructions




data Button = Button {
                butLocation :: Point -- location of button
              , butSize :: Point -- size of button
              , butText :: String -- text displayed within button
              }
              deriving Eq

singleButton :: Button -- button that starts a singleplayer game
singleButton = Button (0, 100) buttonSize "Singleplayer"

multiButton :: Button -- button that starts a multiplayer game
multiButton = Button (0, -100) buttonSize "Multiplayer"