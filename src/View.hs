module View where



import ViewableClass
import DataTypes
import Graphics.Gloss
import Constants
import General  




view :: GameState -> IO Picture
view gstate = return (viewPure gstate)

viewPure :: GameState -> Picture
viewPure gstate@(GameState { status = PreStart }) -- add instructions
    = pictures [viewBasicStuff gstate, 
                case player2 gstate of
                Just p -> viewInstructions Singleplayer
                _      -> viewInstructions Multiplayer]
viewPure gstate = viewBasicStuff gstate -- otherwise, just draw: 1. the menu, or 2. all entities, the score and the status (if any)

viewBasicStuff :: GameState -> Picture
viewBasicStuff menu@(Menu {}) = pictures [mkPicture menu singleButton, mkPicture menu multiButton]
viewBasicStuff gstate@(GameState {}) = pictures pics
  where
    pics = steenPics ++ alienPics ++ bulletPics ++ alienBulletPics ++ [playerPic, player2Pic, scorePic, highscorePic, statusPic]

    playerPic = pict (player gstate)
    player2Pic = case player2 gstate of
                 Just p -> pict p
                 _      -> Blank

    steenPics = map pict (stenen gstate)
    alienPics = map pict (aliens gstate)
    bulletPics = map pict (bullets gstate)
    alienBulletPics = map pict (alienBullets gstate)
    
    scorePic = viewScore (score gstate)
    highscorePic = viewHighscore (highscore gstate)
    statusPic = viewStatus (status gstate)

    pict :: Viewable a => a -> Picture
    pict = mkPicture gstate







viewScore :: Int -> Picture
viewScore s = viewText (250, 250) smallTextScale textColor ("Score: " ++ show s)

viewHighscore :: Int -> Picture
viewHighscore s = translate 190 210 (scale smallTextScale smallTextScale (color textColor (text ("Highscore: " ++ show s))))

viewStatus :: Status -> Picture
viewStatus PreStart = viewText (-180, statusY) bigTextScale textColor "Space to start"
viewStatus Paused = viewText (-300, statusY) bigTextScale textColor "Paused, Esc to resume"
viewStatus GameOver = viewText (-320, statusY) bigTextScale textColor "Game over, M for menu"
viewStatus _ = Blank

viewInstructions :: Mode -> Picture
viewInstructions Singleplayer = pictures [viewText (explanationX, 200) bigTextScale textColor "^ to boost"
                                , viewText (explanationX, 140) bigTextScale textColor "<, > to steer"
                                , viewText (explanationX, 80) bigTextScale textColor "Space to shoot"
                                , viewText (explanationX, 20) bigTextScale textColor "Esc to pause"]
viewInstructions _    = pictures [viewText (explanationX, 220) smallTextScale player1Color "^ to boost"
                                , viewText (explanationX, 180) smallTextScale player1Color "<, > to steer"
                                , viewText (explanationX, 140) smallTextScale player1Color "Space to shoot"
                                , viewText (explanationX, 80) smallTextScale player2Color "W to boost"
                                , viewText (explanationX, 40) smallTextScale player2Color "A, D to steer"
                                , viewText (explanationX, 0) smallTextScale player2Color "V to shoot"
                                , viewText (explanationX, -60) smallTextScale textColor "Esc to pause"]