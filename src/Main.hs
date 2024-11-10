module Main where

import Constants
    ( alienPicsScale,
      boostPicsScale,
      screenWidth,
      screenHeight,
      picturesFolder ) 
import DataTypes ( GameState(Menu) ) 
import View ( view )
import Graphics.Gloss.Interface.IO.Game
    ( scale, black, Display(InWindow), playIO )
import Graphics.Gloss ( scale, black, loadBMP, Display(InWindow) )
import HandleInput ( input )
import HandleStep ( step )




main :: IO ()
main = do -- import all images
          alienPic <- loadBMP "Pictures/Ufo.bmp"
          alienAnimPics <- loadPics ["explosieufo1.bmp", "explosieufo2.bmp", "explosieufo3.bmp", "explosieufo4.bmp", "explosieufo5.bmp"]     
          steenAnimPics <- loadPics ["implosieframe1.bmp", "implosieframe2.bmp", "implosieframe3.bmp","implosieframe4.bmp","implosieframe5.bmp"]
          boostAnimPics <- loadPics ["flame1.bmp", "flame2.bmp", "flame3.bmp"]      

          --  scale the images that have a fixed size to their correct size (the size of the steenAnimPics is not fixed)
          let alienPic' = scale alienPicsScale alienPicsScale alienPic
          let alienAnimPics' = map (scale alienPicsScale alienPicsScale) alienAnimPics
          let boostAnimPics' = map (scale boostPicsScale boostPicsScale) boostAnimPics

          -- then call the playIO function
          playIO (InWindow "Stenen" (screenWidth, screenHeight) (0, 0)) 
                  black                                                           -- Background color
                  144                                                             -- Frames per second (independent of tick rate, as long as fps is kept above tick rate)
                  (Menu alienPic' steenAnimPics alienAnimPics' boostAnimPics')    -- Initial state (the menu)
                  view                                                            -- View function
                  input                                                           -- Input event function
                  step                                                            -- Step function (gets called every frame)
  
  where loadPics = mapM $ loadBMP . (picturesFolder ++)