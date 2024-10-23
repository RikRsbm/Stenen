module Main where

import Controller 
import Constants 
import Model 
import View
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss



main :: IO ()
main = do ufoPic <- loadBMP "Pictures/Ufo.bmp"
          let ufoPic' = scale ufoScale ufoScale ufoPic
          steenAnimPics <- mapM loadBMP ["Pictures/implosieframe1.bmp", "Pictures/implosieframe2.bmp", "Pictures/implosieframe3.bmp","Pictures/implosieframe4.bmp","Pictures/implosieframe5.bmp"]
          boostAnimPics <- mapM loadBMP ["Pictures/flame1.bmp", "Pictures/flame2.bmp", "Pictures/flame3.bmp"]
          let boostAnimPics' = map (scale boostBmpScale boostBmpScale) boostAnimPics
          playIO (InWindow "Stenen" (screenWidth, screenHeight) (0, 0)) 
                  black                                              -- Background color
                  144                                                -- Frames per second, keep above bigUpdatesPerSec (= 60). Otherwise the true amount of bigUpdatesPerSec will become smaller than 60, making the game feel slower 
                  (initialState ufoPic' steenAnimPics boostAnimPics')-- Initial state
                  view                                               -- View function
                  input                                              -- Event function
                  step                                               -- Step function