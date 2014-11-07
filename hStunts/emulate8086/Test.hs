import Data.Word
import Data.Int
import Data.Bits hiding (bit)
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Control.Applicative
--import Control.Arrow
import Control.Monad.State
import Control.Monad.Error
import Control.Lens as Lens
import Control.Concurrent
import Control.Concurrent.MVar
import Test.QuickCheck hiding ((.&.))
import System.Environment
import System.IO
import System.IO.Unsafe

import Emulate
import DosBox
import Parse (getLabels)

--------------------------------------------------------------------------------
testCases =
  [ "add"
  , "bcdcnv"
  , "bitwise"
  , "cmpneg"
  , "control"
--  , "datatrnf"
  , "div"
  , "interrupt"
--  , "jmpmov"
  , "jump1"
--  , "jump2"
  , "mul"
  , "rep"
  , "rotate"
  , "segpr"
  , "shifts"
  , "strings"
  , "sub"
  ]

-- http://orbides.1gb.ru/80186_tests.zip
testTest = forM_ testCases $ \n -> do
  putStr $ "Test " ++ n ++ ": "
  m <- mkSteps . loadTest <$> BS.readFile ("tests/" ++ n ++ ".bin")
  let resName = "tests/res_" ++ n ++ ".bin"
  res <- BS.readFile resName
  case m of
    (Halt,x) -> do
      let v = x ^. heap
          r = BS.pack [v ^. byteAt i | i <- [0xe0000..0xeffff]]
          out = BS.take (BS.length res) r
      print $ out == res
      BS.writeFile (resName ++ ".out") out
    (h,_) -> putStrLn $ "Error " ++ show h

comTest = unsafePerformIO $ loadCom <$> BS.readFile "bushes.com"

main = do
    vid <- newEmptyMVar
    hSetBuffering stdout NoBuffering
    args <- getArgs
    forkIO $ 
{-
            writeIORef st $ flip execState x $ runErrorT $ do
                replicateM_ 10000 $ cachedStep
                clearHist
-}
--            putStrLn "."

        print $ config . videoMVar .~ vid $ config . disassStart .~ f args $ config . verboseLevel .~ 1 $ config . termLength .~ 1000 $ steps .~ 2000000 $ initState
    drawWithFrameBuffer (takeMVar vid) $ return ()

  where
    f [i] = read i
    f _ = 0


comTestMain = do
    st <- loadCom <$> BS.readFile "bushes.com" >>= newIORef
{-
    framebuffer <- Vec.replicate (2^16) 0
    Vec.write framebuffer (f initI) 1
    Prelude.forM_ [0xfff4..0xffff] $ \i -> do
        MVec.write framebuffer i 1
-}
    let framebuffer = do
            x <- readIORef st
            let gs = x ^. heap16 0x6 . ann . paragraph
                v = x ^. heap
--            assert "err" $ V.length v == 320 * 200
            return $ \x y -> v ^. byteAt (gs + 320 * y + x) 
    drawWithFrameBuffer framebuffer $ do
        x <- readIORef st
        writeIORef st $ flip execState x $ runErrorT $ do
            replicateM_ 10000 $ cachedStep
            clearHist
        putStrLn "."

--------------------------------------------------------------------------------

loadSegment = 0x100 -- can be arbitrary > 0

initState = unsafePerformIO $ do
    l <- getLabels
    loadExe l loadSegment <$> BS.readFile "../restunts/stunts/game.exe"

eval_ = flip evalState initState . runErrorT
exec = flip execState initState . runErrorT

eval :: Machine () -> Word16
eval s = either f undefined v
  where
    f Halt = s' ^. ax
    f e = error $ show e
    (v, s') = flip runState initState $ runErrorT $ s >> forever cachedStep --(void step_)

call :: String -> Machine ()
call name = do
    let Just (seg, ipInit) = lookup name symbols
    push $ noAnn 0x0000
    push $ noAnn 0x0000
    ds .= dataSegment
    cs .= segments V.! seg
    ip .= ipInit
  where
    dataSegment = 0x2b77 + loadSegment

    segments :: V.Vector Word16
    segments = V.fromList $ map (+ loadSegment) [0, 0, 0, 0, 0,  0, 0x15f2{-?-}, 0, 0, 0,  0, 0x1ea0{-?-}, 0x1ea2]

    symbols =
        [ (,) "sin"               (12, 0x3cbe)    -- 0x24f6e - hs
        , (,) "cos"               (12, 0x3d0c)
        , (,) "polarAngle"        (12, 0x2e)      -- 0x212de - hs
        , (,) "polarRadius2D"     (12, 0x1696)
        , (,) "polarRadius3D"     (11, 0x8)       -- 0x21298 - hs
        , (,) "rectComparePoint"  ( 6, 0xe) -- 0x187be - hs   -- x * 16 + y = 0x15ee2
        ]


--------------------------------------------------------------------------------

fromIntegral' = fromIntegral . asSigned

tests = do
    quickCheck $ \i -> eval (call "sin" @. (i :: Word16)) == sin_fast i
    quickCheck $ \i -> eval (call "cos" @. (i :: Word16)) == cos_fast i

    quickCheck $ \i j -> eval (call "polarAngle" @. (i :: Word16) @. (j :: Word16)) == fromIntegral (polarAngle (fromIntegral' i) (fromIntegral' j))
    quickCheck $ \i j -> eval (call "polarRadius2D" @. (i :: Word16) @. (j :: Word16)) == fromIntegral (polarRadius2D (fromIntegral' i) (fromIntegral' j))
--    q3d
{-
q3d = quickCheck $ \i_ j_ k_ -> let
        i = fromIntegral (i_ :: Int8)
        j = fromIntegral (j_ :: Int8)
        k = fromIntegral (k_ :: Int8)
        v = V.fromList [i,j,k] :: Vect
    in {-polarRadius2D (fromIntegral' i) (fromIntegral' j) == 0x8000
     || -} eval (call "polarRadius3D" @. v) == fromIntegral (polarRadius3D $ V.map fromIntegral' v)

qrp = quickCheck $ \i j ->
        eval (call "rectComparePoint" @. (V.fromList [i,j] :: Vect)) == 1
-}

------------------------

sintab :: V.Vector Word16
sintab = V.fromList [0, 101, 201, 302, 402, 503, 603, 704, 804, 904, 1005, 1105, 1205, 1306, 1406, 1506, 1606, 1706, 1806, 1906, 2006, 2105, 2205, 2305, 2404, 2503, 2603, 2702, 2801, 2900, 2999, 3098, 3196, 3295, 3393, 3492, 3590, 3688, 3786, 3883, 3981, 4078, 4176, 4273, 4370, 4467, 4563, 4660, 4756, 4852, 4948, 5044, 5139, 5235, 5330, 5425, 5520, 5614, 5708, 5803, 5897, 5990, 6084, 6177, 6270, 6363, 6455, 6547, 6639, 6731, 6823, 6914, 7005, 7096, 7186, 7276, 7366, 7456, 7545, 7635, 7723, 7812, 7900, 7988, 8076, 8163, 8250, 8337, 8423, 8509, 8595, 8680, 8765, 8850, 8935, 9019, 9102, 9186, 9269, 9352, 9434, 9516, 9598, 9679, 9760, 9841, 9921, 10001, 10080, 10159, 10238, 10316, 10394, 10471, 10549, 10625, 10702, 10778, 10853, 10928, 11003, 11077, 11151, 11224, 11297, 11370, 11442, 11514, 11585, 11656, 11727, 11797, 11866, 11935, 12004, 12072, 12140, 12207, 12274, 12340, 12406, 12472, 12537, 12601, 12665, 12729, 12792, 12854, 12916, 12978, 13039, 13100, 13160, 13219, 13279, 13337, 13395, 13453, 13510, 13567, 13623, 13678, 13733, 13788, 13842, 13896, 13949, 14001, 14053, 14104, 14155, 14206, 14256, 14305, 14354, 14402, 14449, 14497, 14543, 14589, 14635, 14680, 14724, 14768, 14811, 14854, 14896, 14937, 14978, 15019, 15059, 15098, 15137, 15175, 15213, 15250, 15286, 15322, 15357, 15392, 15426, 15460, 15493, 15525, 15557, 15588, 15619, 15649, 15679, 15707, 15736, 15763, 15791, 15817, 15843, 15868, 15893, 15917, 15941, 15964, 15986, 16008, 16029, 16049, 16069, 16088, 16107, 16125, 16143, 16160, 16176, 16192, 16207, 16221, 16235, 16248, 16261, 16273, 16284, 16295, 16305, 16315, 16324, 16332, 16340, 16347, 16353, 16359, 16364, 16369, 16373, 16376, 16379, 16381, 16383, 16384, 16384]

sin_fast, cos_fast :: Word16 -> Word16
sin_fast s = case (s `shiftR` 8) .&. 3 of
    0 -> sintab V.! c
    1 -> sintab V.! (0x100 - c)
    2 -> - sintab V.! c
    3 -> - sintab V.! (0x100 - c)
  where
    c = fromIntegral (s .&. 0xFF)

cos_fast = sin_fast . (+ 0x100)

type Matrix = V.Vector Word16  -- 11 21 31 12 22 32 13 23 33
                               -- 0  1  2  3  4  5  6  7  8
type Vect = V.Vector Word16

mat_mul_vector :: Vect -> Matrix -> Vect
mat_mul_vector invec m = V.fromList
    [ invec V.! 0 * m V.! 0 + invec V.! 1 * m V.! 3 + invec V.! 2 * m V.! 6
    , invec V.! 0 * m V.! 1 + invec V.! 1 * m V.! 4 + invec V.! 2 * m V.! 7
    , invec V.! 0 * m V.! 2 + invec V.! 1 * m V.! 5 + invec V.! 2 * m V.! 8
    ]

polarAngle :: Int -> Int -> Int
polarAngle 0 0 = 0 --undefined
polarAngle z y = case (z < 0, y < 0, z_ > y_) of
    (False, False, False) ->  result
    (False, False,  True) -> -result + 0x100
    (False,  True, False) -> -result + 0x200
    (False,  True,  True) ->  result + 0x100
    ( True, False, False) -> -result
    ( True, False,  True) ->  result - 0x100
    ( True,  True, False) ->  result - 0x200
    ( True,  True,  True) -> -(result + 0x100)
  where
    y_ = abs y
    z_ = abs z
    (z', y') = if z_ > y_ then (y_, z_) else (z_, y_)

    result
        | z == y = 0x80
        | otherwise = atantable V.! index

    index = round ((fromIntegral z' `shiftL` 16) `div` y') `shiftR` 8

    round x | (x .&. 0xFF) >= 0x80 = x + 0x100
            | otherwise            = x

atantable :: V.Vector Int
atantable = V.fromList [0, 1, 1, 2, 3, 3, 4, 4, 5, 6, 6, 7, 8, 8, 9, 10, 10, 11, 11, 12, 13, 13, 14, 15, 15, 16, 16, 17, 18, 18, 19, 20, 20, 21, 22, 22, 23, 23, 24, 25, 25, 26, 27, 27, 28, 28, 29, 30, 30, 31, 31, 32, 33, 33, 34, 34, 35, 36, 36, 37, 38, 38, 39, 39, 40, 41, 41, 42, 42, 43, 44, 44, 45, 45, 46, 46, 47, 48, 48, 49, 49, 50, 51, 51, 52, 52, 53, 53, 54, 55, 55, 56, 56, 57, 57, 58, 58, 59, 60, 60, 61, 61, 62, 62, 63, 63, 64, 65, 65, 66, 66, 67, 67, 68, 68, 69, 69, 70, 70, 71, 71, 72, 72, 73, 74, 74, 75, 75, 76, 76, 77, 77, 78, 78, 79, 79, 80, 80, 81, 81, 82, 82, 83, 83, 84, 84, 84, 85, 85, 86, 86, 87, 87, 88, 88, 89, 89, 90, 90, 91, 91, 91, 92, 92, 93, 93, 94, 94, 95, 95, 96, 96, 96, 97, 97, 98, 98, 99, 99, 99, 100, 100, 101, 101, 102, 102, 102, 103, 103, 104, 104, 104, 105, 105, 106, 106, 106, 107, 107, 108, 108, 108, 109, 109, 110, 110, 110, 111, 111, 112, 112, 112, 113, 113, 113, 114, 114, 115, 115, 115, 116, 116, 116, 117, 117, 118, 118, 118, 119, 119, 119, 120, 120, 120, 121, 121, 121, 122, 122, 122, 123, 123, 123, 124, 124, 124, 125, 125, 125, 126, 126, 126, 127, 127, 127, 128, 128]

polarRadius2D :: Int -> Int -> Int
polarRadius2D z y = g $ f $ abs $ polarAngle z y
  where
    f r | r >= 0x100 = -(r - 0x200)
        | otherwise = r

    g r | r <= 0x80 = (abs y `shiftL` 14) `div` fromIntegral' (cos_fast $ fromIntegral r)
        | otherwise = (abs z `shiftL` 14) `div` fromIntegral' (sin_fast $ fromIntegral r)

polarRadius3D :: V.Vector Int -> Int
polarRadius3D vec = polarRadius2D (polarRadius2D (vec V.! 0) (vec V.! 1)) (vec V.! 2)


