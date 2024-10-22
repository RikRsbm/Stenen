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
          pictures <- mapM loadBMP ["Pictures/implosieframe1.bmp", "Pictures/implosieframe2.bmp", "Pictures/implosieframe3.bmp","Pictures/implosieframe4.bmp","Pictures/implosieframe5.bmp"]
          playIO (InWindow "Stenen" (screenWidth, screenHeight) (0, 0)) 
                  black                       -- Background color
                  300                         -- Frames per second
                  (initialState ufo' pictures)-- Initial state
                  view                        -- View function
                  input                       -- Event function
                  step                        -- Step function