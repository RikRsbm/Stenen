{-# LANGUAGE InstanceSigs #-}
module HasImplosionAnimationClass where




import DataTypes
import Constants




-- this class generalizes the objects that have an implosion animation



class HasImplosionAnimation a where
    dieState :: a -> DieState -- state of the object (alive, dying, or dead)
    replaceState :: a -> DieState -> a -- replaces the state of the object by the provided state

    updateImplosionAnim :: Float -> a -> a -- updates the implosion animation if enough time has passed
    updateImplosionAnim secs a =
        case dieState a of
            Dying frame time -- if it was already dying
                | time + secs > timePerImplosionFrame -> -- if it's time for a new implosion frame, update the animation
                    replaceState a $ case frame of
                        x | x == maxBound -> Dead -- if it was at the last frame, the object is now officially dead
                        other             -> Dying (succ other) (time + secs - timePerImplosionFrame) -- otherwise, choose the next frame
                | otherwise ->
                    replaceState a (Dying frame (time + secs))
            Alive -> -- if it was alive, set the animation to the first frame
                replaceState a (Dying minBound 0) 
            -- case for Dead isnt needed, since dead ones get filtered out immediately after becoming dead


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