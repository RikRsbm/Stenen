{-# LANGUAGE InstanceSigs #-}
module HasAnimationClass where



import DataTypes
import Constants
import HasImplosionAnimationClass


class HasAnimation a where
    updateAnim :: Float -> a -> a

instance HasAnimation Player where
    updateAnim :: Float -> Player -> Player
    updateAnim secs p@(Player { boostState = BoostFrame x time }) 
        | time + secs > timePerBoostFrame = p { boostState = BoostFrame (case x of 
                                                                         Two2 -> Zero2
                                                                         other -> succ other) 
                                                            (time + secs - timePerBoostFrame) }
        | otherwise                       = p { boostState = BoostFrame x (time + secs) }
    updateAnim secs p = p -- not boosting

instance HasAnimation Steen where
    updateAnim :: Float -> Steen -> Steen
    updateAnim = updateImplosionAnim

instance HasAnimation Alien where
    updateAnim :: Float -> Alien -> Alien
    updateAnim = updateImplosionAnim