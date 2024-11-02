module Main where

import Constants 
import DataTypes 
import View
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import HandleInput
import HandleStep



main :: IO ()
main = do ufoPic <- loadBMP "Pictures/Ufo.bmp"
          let ufoPic' = scale ufoScale ufoScale ufoPic
          ufoAnimPics <- mapM loadBMP ["Pictures/explosieufo1.bmp", "Pictures/explosieufo2.bmp", "Pictures/explosieufo3.bmp", "Pictures/explosieufo4.bmp", "Pictures/explosieufo5.bmp"]
          let ufoAnimPics' = map (scale ufoScale ufoScale) ufoAnimPics
          steenAnimPics <- mapM loadBMP ["Pictures/implosieframe1.bmp", "Pictures/implosieframe2.bmp", "Pictures/implosieframe3.bmp","Pictures/implosieframe4.bmp","Pictures/implosieframe5.bmp"]
          boostAnimPics <- mapM loadBMP ["Pictures/flame1.bmp", "Pictures/flame2.bmp", "Pictures/flame3.bmp"]
          let boostAnimPics' = map (scale boostPicsScale boostPicsScale) boostAnimPics
          playIO (InWindow "Stenen" (screenWidth, screenHeight) (0, 0)) 
                  black                                                           -- Background color
                  144                                                             -- Frames per second, keep above bigUpdatesPerSec (= 60). Otherwise the true amount of bigUpdatesPerSec will become smaller than 60, making the game feel slower 
                  (Menu ufoPic' steenAnimPics ufoAnimPics' boostAnimPics')        -- Initial state
                  view                                                            -- View function
                  input                                                           -- Event function
                  step                                                            -- Step function
  where ufoScale = 2 * alienRadius / ufoBmpSize