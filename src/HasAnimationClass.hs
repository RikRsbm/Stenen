{-# LANGUAGE InstanceSigs #-}
module HasAnimationClass where



import DataTypes
import Constants




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
    updateAnim secs s@(Steen { sState = (ExplosionState frame time) })
        | time + secs > timePerSteenImplosionFrame
            = s { sState = case frame of 
                           x | x == maxBound -> Dead
                           other             -> ExplosionState (succ other) (time + secs - timePerSteenImplosionFrame) }
        | otherwise 
            = s { sState = ExplosionState frame (time + secs) }
    updateAnim secs s@(Steen { sState = Alive })
        = s { sState = ExplosionState minBound 0 }

instance HasAnimation Alien where
    updateAnim :: Float -> Alien -> Alien
    updateAnim secs a@(Alien { aState = (ExplosionState frame time) })
        | time + secs > timePerAlienImplosionFrame 
            = a { aState = case frame of 
                           x | x == maxBound -> Dead
                           other             -> ExplosionState (succ other) (time + secs - timePerAlienImplosionFrame) }
        | otherwise 
            = a { aState = ExplosionState frame (time + secs) }
    updateAnim secs a@(Alien { aState = Alive })
        = a { aState = ExplosionState minBound 0 }