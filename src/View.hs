module View where



import ViewableClass
import DataTypes
import Graphics.Gloss
import Constants
import General  



-- this module contains the view functions (which turn all objects and texts into pictures)



view :: GameState -> IO Picture -- gets called every frame
view gstate = return (viewPure gstate)

viewPure :: GameState -> Picture -- pure version of 'view'
viewPure gstate@(GameState { status = PreStart }) -- if the game has not yet started, add instructions
    = pictures [viewBasicStuff gstate, 
                viewInstructions $ maybe Singleplayer (const Multiplayer) (player2 gstate)]
viewPure gstate = viewBasicStuff gstate 

viewBasicStuff :: GameState -> Picture -- turns everything that should be on the screen into one picture
viewBasicStuff menu@(Menu {}) -- for the menu, only draw the two buttons
    = pictures [mkPicture menu singleButton, mkPicture menu multiButton]
viewBasicStuff gstate@(GameState {}) -- for the actual game, draw all pictures from the pics list
    = pictures pics
  where
    pics = steenPics ++ alienPics ++ bulletPics ++ alienBulletPics ++ [playerPic, player2Pic, scorePic, highscorePic, statusPic]

    -- the lines below turn everything into pictures

    playerPic = pict (player gstate)
    player2Pic = maybe Blank pict (player2 gstate)   -- if player2 exists, turn it into a picture

    steenPics = map pict (stenen gstate) 
    alienPics = map pict (aliens gstate)
    bulletPics = map pict (bullets gstate)
    alienBulletPics = map pict (alienBullets gstate)
    
    scorePic = viewScore (score gstate)
    highscorePic = viewHighscore (highscore gstate)
    statusPic = viewStatus (status gstate)

    pict :: Viewable a => a -> Picture
    pict = mkPicture gstate





viewScore :: Int -> Picture -- turns the score into a picture
viewScore s = viewText (250, 250) smallTextScale textColor ("Score: " ++ show s)

viewHighscore :: Int -> Picture -- turns the highscore into a picture
viewHighscore s = translate 190 210 (scale smallTextScale smallTextScale (color textColor (text ("Highscore: " ++ show s))))

viewStatus :: GameStatus -> Picture -- turns the status into a picture
viewStatus PreStart = viewText (-180, statusY) bigTextScale textColor "Enter to start"
viewStatus Paused = viewText (-300, statusY) bigTextScale textColor "Paused, Esc to resume"
viewStatus GameOver = viewText (-320, statusY) bigTextScale textColor "Game over, M for menu"
viewStatus _ = Blank

viewInstructions :: Mode -> Picture -- turns the instructions into a picture
viewInstructions Singleplayer = pictures [viewText (instructionX, 200) bigTextScale textColor "^ to boost"
                                , viewText (instructionX, 140) bigTextScale textColor "<, > to steer"
                                , viewText (instructionX, 80) bigTextScale textColor "Space to shoot"
                                , viewText (instructionX, 20) bigTextScale textColor "Esc to pause"]
viewInstructions _    = pictures [viewText (instructionX, 220) smallTextScale player1Color "^ to boost"
                                , viewText (instructionX, 180) smallTextScale player1Color "<, > to steer"
                                , viewText (instructionX, 140) smallTextScale player1Color "Space to shoot"
                                , viewText (instructionX, 80) smallTextScale player2Color "W to boost"
                                , viewText (instructionX, 40) smallTextScale player2Color "A, D to steer"
                                , viewText (instructionX, 0) smallTextScale player2Color "V to shoot"
                                , viewText (instructionX, -60) smallTextScale textColor "Esc to pause"]