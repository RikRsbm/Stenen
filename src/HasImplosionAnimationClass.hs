{-# LANGUAGE InstanceSigs #-}
module HasImplosionAnimationClass where




import DataTypes
import Constants




class HasImplosionAnimation a where
    dieState :: a -> DieState
    replaceState :: a -> DieState -> a

    updateImplosionAnim :: Float -> a -> a
    updateImplosionAnim secs a =
        case dieState a of
            Dying frame time
                | time + secs > timePerImplosionFrame ->
                    replaceState a $ case frame of
                        x | x == maxBound -> Dead
                        other -> Dying (succ other) (time + secs - timePerImplosionFrame)
                | otherwise ->
                    replaceState a (Dying frame (time + secs))
            Alive ->
                replaceState a (Dying minBound 0) 
            -- case for dead isnt needed since dead ones get filtered out immediately after becoming dead


instance HasImplosionAnimation Steen where
    dieState :: Steen -> DieState
    dieState = sState

    replaceState :: Steen -> DieState -> Steen
    replaceState s ds = s { sState = ds}

instance HasImplosionAnimation Alien where
    dieState :: Alien -> DieState
    dieState = aState

    replaceState :: Alien -> DieState -> Alien
    replaceState a ds = a { aState = ds }