{-# LANGUAGE InstanceSigs #-}
module HasAnimationClass where



import DataTypes
    ( ZeroToTwo(Zero2, Two2),
      BoostState(BoostFrame),
      Alien,
      Steen,
      Player(Player, boostState) )
import Constants ( ticksPerBoostFrame, gameTicksPerSec )
import HasImplosionAnimationClass
    ( HasImplosionAnimation(updateImplosionAnim) )



-- this class generalizes the objects that have an animation



class HasAnimation a where
    updateAnim :: Float -> a -> a -- update the animation if enough ticks have passed 


instance HasAnimation Player where
    updateAnim :: Float -> Player -> Player
    updateAnim secs p@(Player { boostState = BoostFrame x time }) -- if the player is boosting
        | time + secs > timePerBoostFrame -- if it's time for a new boost frame, update the animation
            = p { boostState = BoostFrame (case x of 
                                           Two2 -> Zero2 -- if it's the last frame, return to the first
                                           other -> succ other) -- otherwise, choose the next frame
                               (time + secs - timePerBoostFrame) }
        | otherwise -- if not enough time has passed, do nothing
            = p { boostState = BoostFrame x (time + secs) }
      where 
        timePerBoostFrame = ticksPerBoostFrame / gameTicksPerSec
    updateAnim secs p = p -- if the player is not boosting, do nothing
    

instance HasAnimation Steen where
    updateAnim :: Float -> Steen -> Steen
    updateAnim = updateImplosionAnim 


instance HasAnimation Alien where
    updateAnim :: Float -> Alien -> Alien
    updateAnim = updateImplosionAnim