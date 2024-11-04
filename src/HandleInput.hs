{-# LANGUAGE NamedFieldPuns #-}
module HandleInput where





import DataTypes
import Graphics.Gloss.Interface.IO.Game
import Data.List
import General
import MorePlayerLogic





-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)


inputKey :: Event -> GameState -> GameState
inputKey (EventKey (MouseButton LeftButton) Down _ p) menu@(Menu {}) = newState
  where
    but = find (withinButtonBounds p) [singleButton, multiButton]

    newState | but == Just singleButton = initState
             | but == Just multiButton  = multState
             | otherwise                = menu

    initState = initialState (ufoPic menu) (steenAnimPics menu) (ufoAnimPics menu) (boostAnimPics menu)
    multState = initState { player2 = Just initialPlayer2 }

inputKey e@(EventKey key Down _ _) gstate@(GameState { status, player, player2 }) 
    | key == Char 'm'            && status == GameOver 
        = Menu (ufoPic gstate) (steenAnimPics gstate) (ufoAnimPics gstate) (boostAnimPics gstate)
    | key == SpecialKey KeySpace && status == PreStart = inputKey e (gstate { status = Running })
    | key == SpecialKey KeySpace && status == Running  = playerShoots gstate player
    | key == Char 'v'            && status == Running  = maybe gstate (playerShoots gstate) player2
    | key == SpecialKey KeyEsc   && status == Running  = gstate { status = Paused }
    | key == SpecialKey KeyEsc   && status == Paused   = gstate { status = Running }
    | key == SpecialKey KeyUp                          = gstate { player = player { forwardPressed = True } }
    | key == SpecialKey KeyLeft                        = gstate { player = player { leftPressed = True } }
    | key == SpecialKey KeyRight                       = gstate { player = player { rightPressed = True } }
    | key == Char 'w'                                  = gstate { player2 = (\p -> p { forwardPressed = True }) <$> player2 }
    | key == Char 'a'                                  = gstate { player2 = (\p -> p { leftPressed = True }) <$> player2 }
    | key == Char 'd'                                  = gstate { player2 = (\p -> p { rightPressed = True }) <$> player2 }
    | otherwise                                        = gstate

inputKey (EventKey key Up _ _) gstate@(GameState { status, player, player2 })
    | key == SpecialKey KeyUp                          = gstate { player = player { forwardPressed = False } }
    | key == SpecialKey KeyLeft                        = gstate { player = player { leftPressed = False } }
    | key == SpecialKey KeyRight                       = gstate { player = player { rightPressed = False } }
    | key == Char 'w'                                  = gstate { player2 = (\p -> p { forwardPressed = False }) <$> player2 }
    | key == Char 'a'                                  = gstate { player2 = (\p -> p { leftPressed = False }) <$> player2 }
    | key == Char 'd'                                  = gstate { player2 = (\p -> p { rightPressed = False }) <$> player2 }
    | otherwise                                        = gstate

inputKey _ gstate = gstate