{-# LANGUAGE NamedFieldPuns #-}
module HandleInput where





import DataTypes
import Graphics.Gloss.Interface.IO.Game
import Data.List
import General
import MorePlayerLogic



--this module handles user input



input :: Event -> GameState -> IO GameState -- gets called when user presses a key
input e gstate = return (inputKey e gstate)



inputKey :: Event -> GameState -> GameState -- pure version of 'input'


inputKey (EventKey (MouseButton LeftButton) Down _ point) menu@(Menu {}) -- if the menu is open and the user clicks somewhere on the screen
    = newState     
  where
    but = find (withinButtonBounds point) [singleButton, multiButton] -- check if a button is clicked

    newState | but == Just singleButton = initState -- if singleplayer button is clicked, start singleplayer game
             | but == Just multiButton  = multState -- if multiplayer button is clicked, start multiplayer game
             | otherwise                = menu      -- else, do nothing

    initState = initialSingleplayerState (alienPic menu) (steenAnimPics menu) (alienAnimPics menu) (boostAnimPics menu)
    multState = initState { player2 = Just initialPlayer2 }


inputKey e@(EventKey key Down _ _) gstate@(GameState { status, player, player2 }) -- if the actual game is open (which is everything except the menu) and the user presses a key
    -- if the game is over and the user presses 'm', return to the menu
    | status == GameOver && key == Char 'm'            
        = Menu (alienPic gstate) (steenAnimPics gstate) (alienAnimPics gstate) (boostAnimPics gstate)

    -- if the game has yet to start and the user presser Enter, start the game
    | status == PreStart && key == SpecialKey KeyEnter = gstate { status = Running}

    -- if the game is paused and the user presses Esc, resume the game
    | status == Paused   && key == SpecialKey KeyEsc   = gstate { status = Running }

    -- if the game is running:
    | status == Running                                = case key of
        SpecialKey KeyEsc   -> gstate { status = Paused }                                                     -- pause the game
        SpecialKey KeySpace -> playerShoots gstate player                                                     -- player1 shoots
        Char 'v'            -> maybe gstate (playerShoots gstate) player2                                     -- if player2 exists: player2 shoots
        SpecialKey KeyUp    -> gstate { player = player { boostState = BoostFrame Zero2 0 } }                 -- player1 boosts, set boostState to first frame of animation
        SpecialKey KeyLeft  -> gstate { player = player { leftPressed = True } }                              -- player1 steers left
        SpecialKey KeyRight -> gstate { player = player { rightPressed = True } }                             -- player1 steers right
        Char 'w'            -> gstate { player2 = (\p -> p { boostState = BoostFrame Zero2 0 }) <$> player2 } -- if player2 exists: player2 boosts, set boostState to first frame of animation
        Char 'a'            -> gstate { player2 = (\p -> p { leftPressed = True }) <$> player2 }              -- if player2 exists: player2 steers left
        Char 'd'            -> gstate { player2 = (\p -> p { rightPressed = True }) <$> player2 }             -- if player2 exists: player2 steers right
        _                   -> gstate
    | otherwise                                        = gstate


inputKey (EventKey key Up _ _) gstate@(GameState { status, player, player2 }) -- if the actual game is open and the user releases a key
    | key == SpecialKey KeyUp    = gstate { player = player { boostState = NotBoosting } }                 -- player1 stops boosting
    | key == SpecialKey KeyLeft  = gstate { player = player { leftPressed = False } }                      -- player1 stops steering left
    | key == SpecialKey KeyRight = gstate { player = player { rightPressed = False } }                     -- player1 stops steering right
    | key == Char 'w'            = gstate { player2 = (\p -> p { boostState = NotBoosting }) <$> player2 } -- if player2 exists: player2 stops boosting
    | key == Char 'a'            = gstate { player2 = (\p -> p { leftPressed = False }) <$> player2 }      -- if player2 exists: player2 stops steering left
    | key == Char 'd'            = gstate { player2 = (\p -> p { rightPressed = False }) <$> player2 }     -- if player2 exists: player2 stops steering right
    | otherwise                  = gstate


inputKey _ gstate = gstate