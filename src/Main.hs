module Main where

import Controller 
import Constants 
import Model 
import View
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss



main :: IO ()
main = do ufo <- loadBMP "Pictures/Ufo.bmp"
          let ufo' = scale ufoScale ufoScale ufo
          playIO (InWindow "Stenen" (screenWidth, screenHeight) (0, 0)) 
                  black                      -- Background color
                  60                         -- Frames per second
                  (initialState ufo')        -- Initial state
                  view                       -- View function
                  input                      -- Event function
                  step                       -- Step function