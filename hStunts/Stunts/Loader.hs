module Stunts.Loader where

import Data.Char (chr)
import Control.Applicative
import Control.Monad
import Data.Binary as B
import Data.Binary.Get as B
import Data.Bits
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.Int
import Data.IntMap (Key)
import qualified Data.IntMap as IM
import Data.List
import qualified Data.Trie as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import Stunts.Color

getString :: Int -> Get String
getString = fmap (SB8.unpack . SB8.takeWhile (/= '\0')) . getByteString

getString' :: Get String
getString' = do
    c <- chr . fromIntegral <$> getWord8
    if c == '\0'
        then return []
        else (c:) <$> getString'

getWord :: Get Word32
getWord = getWord32le

getWord16 :: Get Word16
getWord16 = getWord16le

getUByte :: Get Word8
getUByte = B.get :: Get Word8

getInt8 :: Get Int
getInt8 = fromIntegral <$> getUByte :: Get Int

getInt16' :: Get Int16
getInt16' = fromIntegral <$> getWord16le :: Get Int16

getInt16 :: Get Int
getInt16 = fromIntegral <$> getInt16' :: Get Int

getInt' :: Get Int32
getInt' = fromIntegral <$> getWord32le :: Get Int32

getInt :: Get Int
getInt = fromIntegral <$> getInt' :: Get Int

data PrimitiveType
    = Particle
    | Line
    | Polygon
    | Sphere
    | Wheel
    | Ignored
    deriving (Eq,Ord,Show)

data Primitive
    = Primitive
    { prType        :: PrimitiveType
    , prTwoSided    :: Bool
    , prZBias       :: Bool
    , prMaterials   :: [Int]
    , prIndices     :: [Int]
    }
    deriving Show

data Model
    = Model
    { mdVertices    :: [(Float,Float,Float)]
    , mdPrimitives  :: [Primitive]
    }
    deriving Show

getVertex :: Get (Float, Float, Float)
getVertex = do
    x <- getInt16
    y <- getInt16
    z <- getInt16
    let aspectCorrection = 1 -- 6.0/5.0
        f  = fromIntegral :: Int -> Float
        y' = aspectCorrection * f y
    return (f x,y',-f z)

getPrimitive :: Int -> Get Primitive
getPrimitive numPaintJobs = do
    let convertType c
            | c == 1    = (1,Particle)
            | c == 2    = (2,Line)
            | 3 <= c &&
              c <= 10   = (c,Polygon)
            | c == 11   = (2,Sphere)
            | c == 12   = (6,Wheel)
            | otherwise = (0,Ignored)
    (cnt,ptype) <- convertType <$> getInt8
    (twosided,zbias) <- (\i -> (testBit i 0,testBit i 1)) <$> getUByte
    materials <- replicateM numPaintJobs getInt8
    indices <- replicateM cnt getInt8
    return $ Primitive ptype twosided zbias materials indices

getModel :: Get Model
getModel = do
    numVertices <- getInt8
    numPrimitives <- getInt8
    numPaintJobs <- getInt8
    getInt8
    vertices <- replicateM numVertices getVertex
    _cullFront <- replicateM numPrimitives getWord
    _cullBack <- replicateM numPrimitives getWord
    primitives <- replicateM numPrimitives (getPrimitive numPaintJobs)
    return $ Model vertices primitives

getResources :: Get [(SB.ByteString, LB.ByteString)]
getResources = do
    _fileLength <- getInt
    numResources <- getInt16
    ids' <- replicateM numResources $ getString 4
    offsets' <- map fromIntegral <$> replicateM numResources getInt
    dat <- getRemainingLazyByteString
    let (ids,offsets) = unzip $ sortBy (\(_,a) (_,b) -> compare a b) $ zip ids' offsets'
        lens = snd $ foldl' (\(p,l) o -> (o,(p-o):l)) ((LB.length dat),[]) $ reverse offsets
    return [(SB8.pack i,LB.take l $ LB.drop o dat) | (i,o,l) <- zip3 ids offsets lens]

readResources :: LB.ByteString -> T.Trie LB.ByteString
readResources dat = T.fromList $ runGet getResources dat

-- bitmap
data Bitmap
    = Bitmap
    { width     :: Int
    , height    :: Int
    , positionX :: Int
    , positionY :: Int
    , image     :: SV.Vector Word8 -- RGBA data
    , unknown1  :: [Word16]
    , unknown2  :: [Word8]
    }
{-
uint16 width
uint16 height
uint16 unknown1[2]
uint16 positionX
uint16 positionY
uint8  unknown2[4]

uint8  image[width * height]
-}
getBitmap :: Get Bitmap
getBitmap = do
    width <- getInt16
    height <- getInt16
    unknown1 <- replicateM 2 getWord16
    positionX <- getInt16
    positionY <- getInt16
    unknown2 <- replicateM 4 getUByte
    image <- case unknown2 of
        [1,2,4,8]   -> replicateM (width * height) ((vgaPal V.!) <$> getInt8)
        [1,2,20,8]  -> do
            img <- replicateM width $ 
                replicateM height ((vgaPal V.!) <$> getInt8)
            return $ concat $ transpose img
        [1,2,36,8]  -> do
            img <- replicateM width $ do
                col <- replicateM height ((vgaPal V.!) <$> getInt8)
                let (lo,hi) = splitAt ((height + 1) `div` 2) col
                    go0 [] a = a
                    go0 (a:xs) b = a : go1 xs b
                    go1 a [] = a
                    go1 a (b:xs) = b : go0 a xs
                return $ go0 lo hi
            return $ concat $ transpose img

        _           -> replicateM (width * height) ((vgaPal V.!) <$> getInt8)
    return $ Bitmap width height positionX positionY (SV.fromList $ concatMap serializeColor image) unknown1 unknown2

data Car
    = Car
    -- Number of gears.
    { gears         :: Int

    -- The gear ratios are overall values, representing the effects both the gearbox
    -- and the final drive gears as well as those of the wheel radius.
    -- car_speed_mph = 256*engine_speed_rpm/gear_ratio
    , gearRatios    :: [Int]

    -- Every byte navigated forward corresponds to increments of 128rpm,
    -- so that byte 61h covers 0...127rpm; 62h, 128...255rpm and so on
    -- There are 103 bytes in total, and so the engine can deliver power over a range of 13184rpm.
    , torqueCurve   :: [Int]

    -- The main function of the parameter is to define up to which rpm value
    -- the "idle rpm torque" will be used instead of the regular torque curve for the second gear and above.
    , idleRpm       :: Int

    -- This may be thought as a special point in the torque curve.
    -- It overrides a section of the curve at low rpms,
    -- in order to better represent the car launch from a standstill.
    , idleRpmTorque :: Int

    -- This is the downshift rpm point used by the automatic transmission.
    , downshiftRpm  :: Int

    -- This is the upshift rpm point used by the automatic transmission.
    , upshiftRpm    :: Int

    -- This parameter is the maximum rpm (the "redline") of the engine.
    , maxRpm        :: Int

    -- Car mass.
    , mass          :: Int

    -- Tells how powerful the car brakes will be.
    , braking       :: Int

    -- This elusive parameter controls aerodynamic resistance encountered by the car when accelerating down a straight.
    , aeroResistance            :: Int

    -- This is the primary handling parameter. Higher values make it possible to take corners at higher speeds without skidding,
    -- and thus raise cornering speeds as well as lower the risk of loss of control
    -- (at the rather small cost of making controlled sliding harder, which sometimes can be an inconvenience in competition racing).
    , grip          :: Int

    -- These values are responsible for modifying the car grip according to the kind of surface it is on.
    -- The four integer values correspond to the four kinds of surface of Stunts
    -- asphalt, dirt, ice and grass, in that order.
    , surfaceGrip   :: [Int]

    -- These four integers set half-width, height and two half-length values for the car.
    -- These are only used for the detection of car-car collisions.
    , collision     :: [Int]

    -- The first triplet corresponds to the front/left wheel;
    -- the other three stand for the remaining ones, ordered clockwise.
    , wheelPos      :: [(Int,Int,Int)]

    -- graphical properties

    , cockpitHeight             :: Int  -- This sets the apparent height from the ground on the inside (F1) view.
    , shiftingKnobPos           :: [(Int,Int)]  -- shifting knob coordinates
    , steeringDot               :: [(Int,Int)] -- 1+33 pairs
    , speedometerCentre         :: (Int,Int)
    , speedometerNeedle         :: [(Int,Int)]
    , digitalSpeedometer        :: [(Int,Int)]
--    , revMeterNeedle            :: [(Int,Int)]

    -- text properties
    , infoText                  :: String
    , scoreboardName            :: String
    }


getCar :: String -> String -> Get Car
getCar carDesc carName = do
    let pos i g = lookAhead $ do
            b <- bytesRead
            skip i
            g
    gears               <- pos 0x26  getInt8
    mass                <- pos 0x28  getInt16
    braking             <- pos 0x2A  getInt16
    idleRpm             <- pos 0x2C  getInt16
    downshiftRpm        <- pos 0x2E  getInt16
    upshiftRpm          <- pos 0x30  getInt16
    maxRpm              <- pos 0x32  getInt16
    gearRatios          <- pos 0x36  (replicateM 6 getInt16)
    shiftingKnobPos     <- pos 0x44  (replicateM 6 ((,) <$> getInt16 <*> getInt16))
    aeroResistance      <- pos 0x5D  getInt16
    idleRpmTorque       <- pos 0x60  getInt16
    torqueCurve         <- pos 0x61  (replicateM 105 getInt8)
    grip                <- pos 0xCA  getInt16
    surfaceGrip         <- pos 0xDC  (replicateM 4 getInt16)
    collision           <- pos 0xEE  (replicateM 4 getInt16)
    cockpitHeight       <- pos 0xF6  getInt16
    wheelPos            <- pos 0xF8  (replicateM 4 ((,,) <$> getInt16 <*> getInt16 <*> getInt16))
    steeringDot         <- pos 0x110 (replicateM 34 ((,) <$> getInt8 <*> getInt8))
    speedometerCentre   <- pos 0x14E ((,) <$> getInt16 <*> getInt16)
    speedometerCount    <- pos 0x152 getInt16
    speedometerNeedle   <- pos 0x154 (replicateM 104 ((,) <$> getInt8 <*> getInt8))
    digitalSpeedometer  <- pos 0x154 (replicateM 3 ((,) <$> getInt8 <*> getInt8))
    return $ Car
        { gears              = gears
        , gearRatios         = gearRatios
        , torqueCurve        = torqueCurve
        , idleRpm            = idleRpm
        , idleRpmTorque      = idleRpmTorque
        , downshiftRpm       = downshiftRpm
        , upshiftRpm         = upshiftRpm
        , maxRpm             = maxRpm
        , mass               = mass
        , braking            = braking
        , aeroResistance     = aeroResistance
        , grip               = grip
        , surfaceGrip        = surfaceGrip
        , collision          = collision
        , wheelPos           = wheelPos
        , cockpitHeight      = cockpitHeight
        , shiftingKnobPos    = shiftingKnobPos
        , steeringDot        = steeringDot
        , speedometerCentre  = speedometerCentre
        , speedometerNeedle  = speedometerNeedle
        , digitalSpeedometer = digitalSpeedometer
        , infoText           = carDesc
        , scoreboardName     = carName
        }

{-
opponent data:
    - files:
        sdosel.pvs (bitmaps)
            opp0 - opp6 : opponent portraits (opp0 is actually the chronometer for the time trial option)
            scrn
            clip
        opponent animations:
            opp?win.pvs, opp?lose.pvs - bitmaps
                op01 - op08 : the actual number of frames within a file varies from 3 to 8
        opp?.pre
            winn, lose - Each byte in these NULL-terminated resources is a numerical index to the op01 ... op08 bitmaps,
            and the overall sequence is the succession of frames.
            sped - Opponent performance
            path - (186 bytes) numerical data resource which function is not yet understood.
-}

{-
data OpponentData
    = OpponentData
    { screen    :: Bitmap
    , clip      :: Bitmap
    , opponents :: [Opponent]
    }

data Opponent
    = Opponent
    { avatar    :: Bitmap
    , winAnim   :: [Bitmap]
    , looseAnim :: [Bitmap]
    , speed     :: OpponentPerformance
    }
data OpponentPerformance
    = OpponentPerformance
    { road          :: Int3 -- paved, dirt and icy road
    , smallCorner   :: Int3 -- paved, dirt and icy small corner
    , largeCorner   :: Int3 -- paved, dirt and icy large corner
    , bankedCorner  :: Int
    , bridge        :: Int
    , slalom        :: Int
    , corkUpDown    :: Int
    , chicane       :: Int
    , loop          :: Int
    , corkLeftRight :: Int
    }
get3Int8 :: Get (Int, Int, Int)
get3Int8 = (,,) <$> getInt8 <*> getInt8 <*> getInt8

getOpponentPerformance :: Get OpponentPerformance
getOpponentPerformance = OpponentPerformance <$> get3Int8 <*> get3Int8 <*> get3Int8
                                             <*> getInt8 <*> getInt8 <*> getInt8 <*> getInt8
                                             <*> getInt8 <*> getInt8 <*> getInt8
-}

readTrack :: SB8.ByteString -> ([(Int, Int, Int, Bool)], [(Key, Int, Int, Bool)])
readTrack dat = (filter filterTerrainItem (idx (@/)), mapTrackItem =<< idx (@=))
  where
    trkData     = map fromEnum (SB.unpack dat)
    idx sel     = [(sel x y,x,y,x @/ y == 0x06) | y <- [0..29], x <- [0..29]]
    x @/ y      = V.fromList ter V.! (y*30+x)
    x @= y      = V.fromList trk V.! ((29-y)*30+x)
    (trk,_:ter) = splitAt 900 trkData

    roadV = [0x04,0x0E,0x18]
    roadH = [0x05,0x0F,0x19]
    map07 = IM.fromList $ [(i,0x67) | i <- [0x27,0x3B,0x62]] ++ zip roadV [0xD0,0xD4,0xD8]
    map09 = IM.fromList $ [(i,0x67) | i <- [0x26,0x3A,0x61]] ++ zip roadV [0xD2,0xD6,0xDA]
    map08 = IM.fromList $ [(i,0x68) | i <- [0x24,0x38,0x5F]] ++ zip roadH [0xD1,0xD5,0xD9]
    map0A = IM.fromList $ [(i,0x68) | i <- [0x25,0x39,0x60]] ++ zip roadH [0xD3,0xD7,0xDB]

    mapTrackItem i@(c,x,y,e)
        -- remove filler elements
        | elem c [0x00,0xFE,0xFD,0xFF] = []
        -- this is composed from two elements
        | c == 0x65 = [(0x67,x,y,e),(0x05,x,y,e)]
        | c == 0x66 = [(0x68,x,y,e),(0x04,x,y,e)]
        -- ramp on brae is replaced with elevated road
        | x @/ y == 0x07 && IM.member c map07 = [(map07 IM.! c,x,y,e)]
        | x @/ y == 0x09 && IM.member c map09 = [(map09 IM.! c,x,y,e)]
        | x @/ y == 0x08 && IM.member c map08 = [(map08 IM.! c,x,y,e)]
        | x @/ y == 0x0A && IM.member c map0A = [(map0A IM.! c,x,y,e)]
        | otherwise = [i]

    filterTerrainItem (c,x,y,_)
        | elem c [0x07,0x09] && elem (x @= y) roadV = False
        | elem c [0x08,0x0A] && elem (x @= y) roadH = False
        | otherwise = True
