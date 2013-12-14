{-# LANGUAGE BangPatterns #-}
module FCurve where

import Data.Bits
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

-- Haskell implementation of Blender FCurve
-- see: source/blender/blenkernel/intern/fcurve.c

{-
  fcurve eval functions:
    static float fcurve_eval_keyframes(FCurve *fcu, BezTriple *bezts, float evaltime)
    void correct_bezpart(float v1[2], float v2[2], float v3[2], float v4[2])
    static int findzero(float x, float q0, float q1, float q2, float q3, float *o)
    static void berekeny(float f1, float f2, float f3, float f4, float *o, int b)
-}
{-
/**
 * Keyframes on F-Curves (allows code reuse of Bezier eval code) and
 * Points on Bezier Curves/Paths are generally BezTriples 
 *
 * \note alfa location in struct is abused by Key system
 *
 * \note vec in BezTriple looks like this:
 * - vec[0][0] = x location of handle 1
 * - vec[0][1] = y location of handle 1
 * - vec[0][2] = z location of handle 1 (not used for FCurve Points(2d))
 * - vec[1][0] = x location of control point
 * - vec[1][1] = y location of control point
 * - vec[1][2] = z location of control point
 * - vec[2][0] = x location of handle 2
 * - vec[2][1] = y location of handle 2
 * - vec[2][2] = z location of handle 2 (not used for FCurve Points(2d))
 */
typedef struct BezTriple {
	float vec[3][3];
	float alfa, weight, radius;	/* alfa: tilt in 3D View, weight: used for softbody goal weight, radius: for bevel tapering */
	short ipo;					/* ipo: interpolation mode for segment from this BezTriple to the next */
	char h1, h2; 				/* h1, h2: the handle type of the two handles */
	char f1, f2, f3;			/* f1, f2, f3: used for selection status */
	char hide;					/* hide: used to indicate whether BezTriple is hidden (3D), type of keyframe (eBezTriple_KeyframeTypes) */
} BezTriple;


/* interpolation modes (used only for BezTriple->ipo) */
typedef enum eBezTriple_Interpolation {
	BEZT_IPO_CONST = 0,	/* constant interpolation */
	BEZT_IPO_LIN = 1,	/* linear interpolation */
	BEZT_IPO_BEZ = 2	/* bezier interpolation */
} eBezTriple_Interpolation;
-}

data Extrapolation
    = EConstant
    | ELinear
    deriving (Show,Eq)

data Interpolation
    = IConstant
    | ILinear
    | IBezier
    deriving (Show,Eq)

data Segment
    = Segment
    { interpolation :: Interpolation
    , leftTime      :: Float
    , leftValue     :: Float
    , time          :: Float
    , value         :: Float
    , rightTime     :: Float
    , rightValue    :: Float
    }
    deriving Show

data FCurve
    = FCurve
    { extrapolation :: Extrapolation
    , keyframes     :: Vector Segment
    }
    | Samples
    { startTime :: Float
    , endTime   :: Float
    , samples   :: Vector Float
    }
    deriving Show

data Value
    = Value
    { name      :: String   -- data path
    , fcurves   :: [FCurve] -- sorted by array indices
    }
    deriving Show

evaluate :: Value -> Float -> [Float]
evaluate v t = map (eval t) $ fcurves v

-- gives the index of the greatest element which is less then the search key
binarySearchBy :: (a -> t -> Ordering) -> V.Vector a -> t -> Int
binarySearchBy cmp vec e = loop 0 (V.length vec)
  where
    loop !l !u
        | u <= l    = l - 1
        | otherwise = case cmp e' e of 
            LT -> loop (k+1) u
            EQ -> k
            GT -> loop l     k
          where
            e' = vec ! k
            k = (u + l) `shiftR` 1

{-
    blender fcurve selection: object name/data path

    implement only channel/keyframes support
-}

interpf :: Float -> Float -> Float -> Float
interpf target origin fac = (fac * target) + (1.0 - fac) * origin;

eval :: Float -> FCurve -> Float
eval t (Samples sTime eTime v)
    -- empty
    | V.null v      = 0 -- HINT: default value or error?
    | t <= sTime    = V.head v
    | t >= eTime    = V.last v
    | otherwise     =   let frac = abs $ t - (fromIntegral $ floor t)
                            idx = floor $ t - sTime
                            val0 = v ! idx
                            val1 = v ! (idx + 1)
                        in if frac == 0
                            then val0
                            else interpf val0 val1 frac
eval t (FCurve e v)
    -- empty
    | V.null v = 0 -- HINT: default value or error?
    -- before first
    | (time $ V.head v) >= t = extrapolate (V.head v) 1 leftTime leftValue
    -- after last
    | (time $ V.last v) <= t = extrapolate (V.last v) (V.length v - 2) rightTime rightValue
    -- in a segment
    | otherwise =   let idx = binarySearchBy (\a b -> time a `compare` b) v t
                    in interpolate (v ! idx) (v ! (idx + 1))
  where
    smallNumber = 1e-8
    extrapolate s@(Segment i _ _ t0 val0 _ _) idx tfun vfun
        -- constant
        | i == IConstant || e == EConstant = val0
        -- linear, linear
        | i == ILinear = case v !? idx of
            Nothing -> val0
            Just (Segment _ _ _ t1 val1 _ _) -> let dx  = t0 - t
                                                    fac = t1 - t0
                                                in if fac == 0
                                                    then val0
                                                    else val0 - (val1 - val0) / fac * dx
        -- linear, bezier
        | otherwise =   let dx  = t0 - t
                            fac = t0 - tfun s
                        in if fac == 0
                            then val0
                            else val0 - (val0 - vfun s) / fac * dx

    interpolate (Segment i _ _ t0 val0 rt0 rval0) (Segment _ lt1 lval1 t1 val1 _ _)
        | abs (t1 - t) < smallNumber = val1
        | otherwise = case i of
            IConstant   -> val0
            ILinear     ->  let fac = t1 - t0
                            in if fac == 0
                                then val0
                                else val0 + (val1 - val0) / fac * (t - t0)
            IBezier     ->  let (t0',val0',rt0',rval0',lt1',lval1',t1',val1') = correct_bezpart (t0,val0,rt0,rval0,lt1,lval1,t1,val1)
                            in case findzero t t0' rt0' lt1' t1' of
                                (b:_) -> berekeny val0' rval0' lval1' val1' b
                                _ -> error "findzero: no solution"

{- The total length of the handles is not allowed to be more
 * than the horizontal distance between (v1-v4).
 * This is to prevent curve loops.
 -}
correct_bezpart :: (Float,Float,Float,Float,Float,Float,Float,Float) -> (Float,Float,Float,Float,Float,Float,Float,Float)
correct_bezpart bez@(v10,v11,v20,v21,v30,v31,v40,v41) =
    if len1 + len2 == 0
        -- if the handles have no length, no need to do any corrections
        then bez
        else if len1 + len2 > len
            -- the two handles cross over each other, so force them apart using the proportion they overlap 
            then
                let !fac  = len / (len1 + len2)
                    !v20' = v10 - fac * h10
                    !v21' = v11 - fac * h11

                    !v30' = v40 - fac * h20
                    !v31' = v41 - fac * h21
                in (v10,v11,v20',v21',v30',v31',v40,v41)
            else bez

  where
    -- calculate handle deltas
    !h10 = v10 - v20
    !h11 = v11 - v21

    !h20 = v40 - v30
    !h21 = v41 - v31

    {- calculate distances: 
     *  - len	= span of time between keyframes
     *	- len1	= length of handle of start key
     *	- len2  = length of handle of end key
     -}
    !len = v40 - v10
    !len1 = abs h10
    !len2 = abs h20

berekeny :: Float -> Float -> Float -> Float -> Float -> Float
berekeny !f1 !f2 !f3 !f4 !t = c0 + t * c1 + t * t * c2 + t * t * t * c3
  where
    !c0 = f1
    !c1 = 3 * (f2 - f1)
    !c2 = 3 * (f1 - 2 * f2 + f3)
    !c3 = f4 - f1 + 3 * (f2 - f3)

sqrt3d d
    | d == 0    = 0
    | d < 0     = -exp(log(-d) / 3)
    | otherwise = exp(log(d) / 3)

-- find root ('zero')
findzero :: Float -> Float -> Float -> Float -> Float -> [Float]
findzero !x !q0 !q1 !q2 !q3 =
    let small = -1e-6
        !c0 = q0 - x
        !c1 = 3 * (q1 - q0)
        !c2 = 3 * (q0 - 2 * q1 + q2)
        !c3 = q3 - q0 + 3 * (q1 - q2)
        result l = [o | o <- l, o >= small && o <= 1.000001]
    in if c3 /= 0
        then
            let !b = c1 / c3
                !c = c0 / c3
                !a = c2 / c3 / 3

                !p = b / 3 - a * a
                !q = (2 * a * a * a - a * b + c) / 2
                !d = q * q + p * p * p

            in case () of
              _ | d > 0 ->
                    let !t = sqrt d
                    in result [sqrt3d(-q + t) + sqrt3d(-q - t) - a]

                | d == 0 ->
                    let !t = sqrt3d(-q)
                    in result [2 * t - a, -t - a]

                | otherwise ->
                    let !phi = acos(-q / sqrt(-(p * p * p)))
                        !t = sqrt(-p)
                        !p' = cos(phi / 3)
                        !q' = sqrt(3 - 3 * p' * p')
                    in result [2 * t * p' - a, -t * (p' + q') - a, -t * (p' - q') - a]
        else
            let !a = c2
                !b = c1
                !c = c0
            in case () of
              _ | a /= 0 ->
                    -- discriminant
                    let !p = b * b - 4 * a * c
                    in if p > 0
                        then
                            let !p' = sqrt p
                            in result [(-b - p') / (2 * a), (-b + p') / (2 * a)]
                        else if (p == 0) then result [-b / (2 * a)] else []
                | b /= 0 -> result [-c / b]
                | c == 0 -> result [0]
                | otherwise -> []
