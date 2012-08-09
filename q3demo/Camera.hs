module Camera where

import Control.Applicative
import Data.Vect
import qualified Data.Vector as V
import FRP.Elerea.Param

userCamera :: Vec3 -> Signal (Float, Float) -> Signal (Bool, Bool, Bool, Bool, Bool)
           -> SignalGen Float (Signal (Vec3, Vec3, Vec3))
userCamera p mposs keyss = fmap (\(pos,target,up,_) -> (pos,target,up)) <$> transfer2 (p,zero,zero,(0,0)) calcCam mposs keyss
  where
    d0 = Vec4 0 (-1) 0 1
    u0 = Vec4 0 0 (-1) 1
    calcCam dt (dmx,dmy) (left,up,down,right,turbo) (p0,_,_,(mx,my)) = (p',p'&+d,u,(mx',my'))
      where
        nil c n = if c then n else zero
        p'  = nil left (v &* (-t)) &+ nil up (d &* t) &+ nil down (d &* (-t)) &+ nil right (v &* t) &+ p0
        k   = if turbo then 500 else 100
        t   = k * realToFrac dt
        mx' = dmx + mx
        my' = dmy + my
        rm  = fromProjective $ rotationEuler $ Vec3 (mx' / 100) (my' / 100) 0
        d   = trim $ rm *. d0 :: Vec3
        u   = trim $ rm *. u0 :: Vec3
        v   = normalize $ d &^ u

rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixZ a .*. rotMatrixX b .*. rotMatrixY (-c)

recordSignalSamples :: Signal Bool -> Signal Bool -> Signal a -> SignalGen p (Signal [a])
recordSignalSamples = transfer3 [] record
  where
    record _ _    True _ _  = [] 
    record _ True _    x xs = x:xs 
    record _ _    _    _ xs = xs 

playbackCamera :: Signal Bool -> Signal Bool -> Signal Float -> Signal [(Vec3, Vec3)] -> SignalGen Float (Signal (Maybe (Vec3, Vec3, Vec3)))
playbackCamera play stop speed recording = do
    let noPath = (V.empty, V.empty)
        trackPath _ play stop waypoints path = if stop then noPath else if play then mkPath waypoints else path
        mkPath waypoints = (camPath, targetPath)
          where
            waypoints' = reverse waypoints
            camPath = extendPath (V.fromList (map fst waypoints'))
            targetPath = extendPath (V.fromList (map snd waypoints'))
        
        stepCamera dtime (camPath, _targetPath) speed t
            | V.length camPath < 4 = 0
            | otherwise            = if t' > tmax - 0.05 then t' - tmax else t'
          where
            t' = proceedOnPath camPath 10 t (dtime * speed)
            tmax = fromIntegral (V.length camPath - 3)
    
    path <- transfer3 noPath trackPath play stop recording
    param <- transfer2 0 stepCamera path speed
    return $ do
        (camPath, targetPath) <- path
        t <- param
        return $ if V.length camPath < 4 then Nothing else Just (samplePath camPath t, samplePath targetPath t, Vec3 0 0 1)

extendPath :: V.Vector Vec3 -> V.Vector Vec3
extendPath ps = V.snoc (V.cons (2 *& ps V.! 0 &- ps V.! 1) ps) (2 *& ps V.! l &- ps V.! (l-1))
  where
    l = V.length ps - 1

proceedOnPath :: V.Vector Vec3 -> Int -> Float -> Float -> Float
proceedOnPath ps prec t d = iterate step t !! prec
  where
    step t = t + d / (len s * fromIntegral prec)
      where
        (i, f) = properFraction t
        s = spline' (ps V.! i) (ps V.! (i+1)) (ps V.! (i+2)) (ps V.! (i+3)) f

samplePath :: V.Vector Vec3 -> Float -> Vec3
samplePath ps t = spline (ps V.! i) (ps V.! (i+1)) (ps V.! (i+2)) (ps V.! (i+3)) f
  where
    (i, f) = properFraction t

spline :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Vec3
spline p0 p1 p2 p3 t = 0.5 *& (2*&p1 &+ (p2 &- p0) &* t &+ (2*&p0 &- 5*&p1 &+ 4*&p2 &- p3) &* t^2 &+ (neg p0 &+ 3*&p1 &- 3*&p2 &+ p3) &* t^3)

spline' :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Vec3
spline' p0 p1 p2 p3 t = 0.5 *& (p2 &- p0 &+ (4*&p0 &- 10*&p1 &+ 8*&p2 &- 2*&p3) &* t &+ ((-3)*&p0 &+ 9*&p1 &- 8*&p2 &+ 3*&p3) &* t^2)
