{-# LANGUAGE NoMonomorphismRestriction, ParallelListComp, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module GameData
    ( StuntsData (..)
    , CarData (..)
    , TrackData (..)
    , readStuntsData
    , scaleFactor
    ) where

import Control.Applicative
--import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Binary.Get as B
--import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Bits
import Data.Word
import Data.Vect.Float
import Data.Vect.Float.Instances ()
import Data.Vect.Float.Util.Quaternion hiding (toU)
import System.Random
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.IntMap as IM
import qualified Data.Trie as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import System.FilePath (takeExtension)

import Graphics.Text.TrueType( decodeFont, Font )

import LambdaCube.GL hiding (Line, Color)
import LambdaCube.GL.Mesh hiding (loadMesh)
import Stunts.Loader
import Stunts.Unpack
import Zip hiding (Archive)
import qualified Zip

--import GameGraphics
import MeshUtil

patch :: LB.ByteString -> LB.ByteString -> LB.ByteString
patch diff source = LB.concat ps `LB.append` rest
  where
    readDiff = do
        w <- getWord16le
        if w == 0 then return []
          else do
            p <- getLazyByteString $ if w .&. 0x8000 == 0 then 2 else 4
            ((fromIntegral $ w .&. 0x7fff, p):) <$> readDiff

    corr ((i, x): xs) = (i - 1, x): xs

    (rest, ps) = mapAccumL appPatch source $ corr $ runGet readDiff diff
    appPatch img (jump, p) = (LB.append p $ LB.drop (LB.length p) rest, unchanged)
      where
        (unchanged, rest) = LB.splitAt jump img

executableImage :: Archive LB.ByteString
executableImage = do
    -- TODO: decide uncompression by extension
    egaCmn  <- unpackResource <$> readZipFile "EGA.CMN"
    mcgaCod <- unpackResource <$> readZipFile "MCGA.COD"
    mcgaDif <- unpackResource <$> readZipFile "MCGA.DIF"
    mcgaHdr <- readZipFile "MCGA.HDR"
    return $ flip runGet mcgaHdr $ do
        skip 2
        bytesInLastPage    <- fromIntegral <$> getWord16le
        pagesInExecutable  <- fromIntegral <$> getWord16le
        skip 2
        paragraphsInHeader <- fromIntegral <$> getWord16le
        let
            executableSize = (pagesInExecutable `shiftL` 9) + if (bytesInLastPage > 0) then bytesInLastPage - 0x1f0 else 0
            headerSize = paragraphsInHeader `shiftL` 4

            pad n a
                | n >= LB.length a = LB.append a $ LB.replicate (n - LB.length a) 0

        return $ pad executableSize $ pad headerSize mcgaHdr `LB.append` patch mcgaDif (egaCmn `LB.append` mcgaCod)

extractDataFromPackedExecutable :: LB.ByteString -> LB.ByteString
extractDataFromPackedExecutable image
    = LB.reverse $ LB.concat $ readInst $ LB.dropWhile (== 0xff) $ LB.reverse $ LB.take packedDataLength $ LB.drop packedDataStart image
  where
    paragraphsInHeader  = readUshortAt 8
    codeSegment         = readUshortAt 22
    exepackOffset       = (paragraphsInHeader + codeSegment) `shiftL` 4
    _destinationLength  = readUshortAt $ exepackOffset + 12
    skipLength          = readUshortAt $ exepackOffset + 14
    packedDataStart     = paragraphsInHeader `shiftL` 4
    packedDataLength    = exepackOffset - ((skipLength - 1) `shiftL` 4) - packedDataStart

    readUshortAt i = fromIntegral (LB.index image i) .|. (fromIntegral (LB.index image $ i+1) `shiftL` 8)

    uc = fromMaybe (error "...") . LB.uncons
    getCount (uc -> (hi, uc -> (low, rest))) = ((fromIntegral hi `shiftL` 8) .|. fromIntegral low, rest)

    readInst (uc -> (opcode, getCount -> (count, rest))) = let
        cont rest = if opcode .&. 1 == 0 then readInst rest else []
      in case (opcode .&. 0xfe, rest) of
        (0xb0, uc -> (fill, rest)) -> LB.replicate count fill: cont rest
        (0xb2, LB.splitAt count -> (block, rest)) -> block: cont rest


type ModelName = SB.ByteString
type Item      = ([ModelName], Float) -- Graphics, Orientation
type Size      = (Int, Int)

trackModelMaps :: LB.ByteString -> V.Vector (Item, Size)
trackModelMaps unpackedCode = trackModelMap
  where
    baseAddress      = 0xabcc
    nameBaseAddress  = 0x9584
    nameSize         = 5
    shapeBaseAddress = 0x764c
    shapeSize        = 22

    trackModelMap = V.generate 0xf8 getTrackModel

    getName i = take 1 $ fst $ fst $ trackModelMap V.! i

    getTrackModel :: Int -> (Item, Size)
    getTrackModel i =
        ( (gr, orientation)
        , ( if sizeFlags .&. 2 == 0 then 1 else 2
          , if sizeFlags .&. 1 == 0 then 1 else 2
          )
        )
      where
        structAddress = baseAddress + fromIntegral i * 14
        otherPartIndex = LB.index unpackedCode (structAddress + 8)
        _materialIndex = LB.index unpackedCode (structAddress + 9)
        shapeAddress = fromIntegral (LB.index unpackedCode $ structAddress + 4) .|. fromIntegral (LB.index unpackedCode $ structAddress + 5) `shiftL` 8
        nameOffset = (shapeAddress - shapeBaseAddress) * nameSize `div` shapeSize + nameBaseAddress
        meshName = SBS.pack $ LB.unpack $ LB.take 4 $ LB.drop nameOffset unpackedCode :: SB.ByteString
        orientation_ = LB.index unpackedCode (structAddress + 3) .&. 0x03
        orientation = fromIntegral ((4 - orientation_) `mod` 4) * (pi/2)
        gr = (if SB.all isAlphaNum meshName then meshName else "")
           : if otherPartIndex == 0 then [] else getName $ fromIntegral otherPartIndex
        sizeFlags = LB.index unpackedCode (structAddress + 11)


-- game specific resource handling
loadRes :: SB.ByteString -> Archive (T.Trie LB.ByteString)
loadRes = fmap (readResources . unpackResource) . readZipFile

loadRawRes :: SB.ByteString -> Archive (T.Trie LB.ByteString)
loadRawRes = fmap readResources . readZipFile

loadMesh :: SB.ByteString -> Archive (T.Trie [Mesh])
loadMesh r = do
    materialMap <- getMaterialMap
    fmap (toMesh materialMap . runGet getModel) <$> loadRes r

loadBitmap :: SB.ByteString -> Archive (T.Trie Bitmap)
loadBitmap r = do
    (vgaPal, _) <- getMaterialMap
    fmap (runGet $ getBitmap vgaPal) <$> loadRes r

loadCarMesh :: SB.ByteString -> Archive (T.Trie [Mesh])
loadCarMesh n = do
    materialMap <- getMaterialMap
    T.mapBy (\k -> Just . toMesh materialMap . fixOp k . runGet getModel) <$> loadRes n
  where
    fixOp k = if k == "car0" then addBottom . fixLambo else id

    -- Remove stray faces from the bottom of the Lamborghini model
    fixLambo md = if n /= "STCOUN.P3S" then md else md'
      where
        miny = minimum [y | (_,y,_) <- mdVertices md]
        ixs = findIndices (\(_,y,_) -> y == miny) (mdVertices md)
        md' = md { mdPrimitives = filter (prOK . prType) $ mdPrimitives md }
        prOK (Polygon is) = null $ intersect ixs is
        prOK _ = True

    -- Add some faces to fill the hole on the bottom of the car models
    addBottom md = md { mdPrimitives = newFaces ++ mdPrimitives md }
      where
        cutHeight = case n of
            "STJAGU.P3S" -> 160
            "STLM02.P3S" -> 320
            "STLANC.P3S" -> 250
            "STP962.P3S" -> 180
            "STPMIN.P3S" -> 100
            _            -> 270

        vs = V.fromList (mdVertices md)
        vec i = let (x,y,z) = vs V.! i in Vec3 x y z

        edges = [e | Primitive { prType = Polygon ixs } <- mdPrimitives md,
                 all ((0 <=) . _1 . vec) ixs, e <- zip ixs (last ixs : ixs)]

        uniqueEdges = go edges
          where
            go [] = []
            go ((i1,i2):es) = case findIndex sameEdge es of
                Just _  -> go (filter (not . sameEdge) es)
                Nothing -> (i1,i2) : go es
              where
                sameEdge (i1',i2') = (i1,i2) == (i1',i2') || (i2,i1) == (i1',i2')

        newFaces = [Primitive (Polygon ixs) False False [57] |
                    (i1,i2) <- uniqueEdges,
                    let (x1,y1,z1) = vs V.! i1,
                    let (x2,y2,z2) = vs V.! i2,
                    y1 < cutHeight || y2 < cutHeight,
                    z1 >= z2,
                    i1' <- V.toList $ V.findIndices (==(-x1,y1,z1)) vs,
                    i2' <- V.toList $ V.findIndices (==(-x2,y2,z2)) vs,
                    let ixs = [i1,i2,i2',i1'],
                    isNewFace ixs]

        isNewFace (i1:i2:i3:_) = (_2 v < 40 && abs (n &. Vec3 0 1 0) > 0.999) || all notOverlapping ps
          where
            notOverlapping (n', v') = abs (n &. n') < 0.999 || abs (n &. normalize (v' &- v)) > 0.001
            (n, v) = plane i1 i2 i3
            ps = [plane i1 i2 i3 | Primitive { prType = Polygon (i1:i2:i3:_) } <- mdPrimitives md]
            plane i1 i2 i3 = (normalize ((v2 &- v1) &^ (v3 &- v1)), v1)
              where
                v1 = vec i1
                v2 = vec i2
                v3 = vec i3
        isNewFace _ = False

loadCarWheels :: SB.ByteString -> Archive [(Vec3, Float, Float)]
loadCarWheels n = do
    m <- fmap (runGet getModel) <$> loadRes n
    return [wheel vl p1 p2 p3 p4 p5 p6 | let Model vl pl = m ! "car0", Primitive { prType = Wheel p1 p2 p3 p4 p5 p6 } <- pl]
  where
    -- wheel pos, wheel width, wheel radius
    wheel vl p1 p2 _p3 p4 _p5 _p6 = ((v p1 + v p4) &* (0.5 * car0ScaleFactor),car0ScaleFactor * (len $ v p1 - v p4),car0ScaleFactor * (len $ v p2 - v p1))
      where
        v i = let (a,b,c) = vl !! i in Vec3 a b c
    car0ScaleFactor = scaleFactor / 20

loadCar :: String -> Archive CarData
loadCar n = do
    materialMap <- getMaterialMap
    mesh <- loadCarMesh $ SB.pack $ "ST" ++ n ++ ".P3S"
    carRes <- loadRawRes $ SB.pack $ "CAR" ++ n ++ ".RES"
    let carSim = runGet (getCar carDesc carName) $ carRes ! "simd"
          where
            lineBreak ']' = '\n'
            lineBreak a = a
            carDesc = map lineBreak $ runGet getString' $ carRes ! "edes"
            carName = runGet getString' $ carRes ! "gnam"
    wheels <- loadCarWheels $ SB.pack $ "ST" ++ n ++ ".P3S"
    bitmapsA <- loadBitmap $ SB.pack $ "STDA" ++ n ++ ".PVS"
    bitmapsB <- loadBitmap $ SB.pack $ "STDB" ++ n ++ ".PVS"
    return $ CarData
        { carMesh     = [transformMesh' (scalingUniformProj4 (1/20) .*. toProj4 pi 0 0 False) m | m <- mesh ! "car0"]
        , wheels      = [(p,w,r,map (transformMesh' (scaling $ Vec3 w r r)) (toMesh materialMap (wheelBase 16))) | (p,w,r) <- wheels]
        , carSimModel = carSim
        , carBitmaps  = T.unionL bitmapsA bitmapsB
        }

loadTrack :: SB.ByteString -> Archive TrackData
loadTrack trkFile = do
    trackModelMap <- trackModelMaps . extractDataFromPackedExecutable <$> executableImage

    game1Map <- loadMesh "GAME1.P3S"
    game2Map <- loadMesh "GAME2.P3S"
    let gameMap = game1Map `T.unionL` game2Map
    (terrainItems,trackItems) <- readTrack <$> readZipFile' trkFile

    let modelIdToMesh :: SB.ByteString -> [Mesh]
        modelIdToMesh n = gameMap ! n

        f ((a,c),_) = (map modelIdToMesh a,c)

        trackMap    = V.imap (\k (a,c) -> (map (map (clampItem k)) a, c)) (V.map f trackModelMap)

        clampItem :: Int -> Mesh -> Mesh
        clampItem i (Mesh attrs prim Nothing) = Mesh (T.mapBy clamp attrs) prim Nothing
          where
            (_, (iw,ih)) = trackModelMap V.! i
            clamp n (A_V3F a)
                | n == "position" = Just $ A_V3F $ if iw == 1 && ih == 1 then go a else a
                | otherwise = Just $ A_V3F a
              where
                minmax = min 512 . max (-512)
                go v = SV.map (\(V3 x y z) -> V3 (minmax x) y (minmax z)) v
            clamp _ a = Just a

        -- TODO: replace this with uniform ground
        patch i ml = ml ++ [modelIdToMesh "high" | i `elem` [0,2,3,4,5,11,12,13,14]]

        terrain = [(toProj4 o x y e,m) | (i,x,y,e) <- terrainItems, let (ml,o) = if i == 0 then ([],0) else trackMap V.! (i + 215), m <- patch i ml] -- U Vec3 Mesh
        track   = [(toProj4' o i x y e,m) | (i,x,y,e) <- trackItems, let (ml,o) = trackMap V.! i, m <- ml] -- U Vec3 Mesh

        startOrientation (c,x,y,e)
            | elem c [0x01, 0x86, 0x93] = Just (pi,toVec3' c x y e &* scaleFactor)    -- North
            | elem c [0xB3, 0x87, 0x94] = Just (0,toVec3' c x y e &* scaleFactor)     -- South
            | elem c [0xB4, 0x88, 0x95] = Just (pi/2,toVec3' c x y e &* scaleFactor)  -- East
            | elem c [0xB5, 0x89, 0x96] = Just (-pi/2,toVec3' c x y e &* scaleFactor) -- West
            | otherwise = Nothing
        startPos = head [i | Just i <- map startOrientation trackItems]

        fenc = game1Map ! "fenc"
        cfen = game1Map ! "cfen"
        fence = [(toProj4 o x y False, fenc) | x <- [1..28], (o,y) <- [(0,0),(pi,29)]] ++
                [(toProj4 o x y False, fenc) | y <- [1..28], (o,x) <- [(pi/2,0),(-pi/2,29)]] ++
                [(toProj4 o x y False, cfen) | (o,x,y) <- [(pi/2,0,0), (0,29,0), (-pi/2,29,29), (pi,0,29)]]

        toProj4' :: Float -> Int -> Int -> Int -> Bool -> Proj4
        toProj4' o i x y e = (orthogonal $ rightOrthoU $ toU o) .*. translation (toVec3' i x y e) .*. scalingUniformProj4 scaleFactor

        toVec3' :: Int -> Int -> Int -> Bool -> Vec3
        toVec3' i x y e = Vec3 (edgeSize * x') (if e then hillHeight else 0) (edgeSize * y')
          where
            f = fromIntegral :: Int -> Float
            (_, (iw,ih)) = trackModelMap V.! i
            x' = f x + (f iw - 1) * 0.5
            y' = f y + (f ih - 1) * 0.5

    clouds <- lift $ replicateM 70 $ do
        let getCloudMesh n = game2Map ! (SB.pack $ "cld" ++ show (1 + n `mod` 3 :: Int))
            getCoord a d = (a', c sin, c cos)
              where
                a' = a*2*pi
                c t = ((50+200*d)*t a'+15)*edgeSize*scaleFactor
        m <- getCloudMesh <$> rnd
        (a,x,z) <- getCoord <$> rnd <*> rnd
        y <- rnd
        return (scalingUniformProj4 (y*0.4+0.3) .*. rotMatrixProj4 a (Vec3 0 1 0) .*. translation (Vec3 x (y*1500+600) z), m)

    return TrackData
        { terrainMesh   = [transformMesh' p m | (p,ml) <- terrain ++ clouds ++ fence, m <- ml]
        , trackMesh     = [transformMesh' p m | (p,ml) <- track, m <- ml]
        , startPosition = startPos
        }

data CarData
    = CarData
    { carMesh       :: [Mesh]
    , wheels        :: [(Vec3,Float,Float,[Mesh])]
    , carSimModel   :: Car
    , carBitmaps    :: T.Trie Bitmap
    }

data TrackData
  = TrackData
  { terrainMesh   :: [Mesh]
  , trackMesh     :: [Mesh]
  , startPosition :: (Float,Vec3)
  }

data StuntsData
    = StuntsData
    { cars          :: [CarData]
    , tracks        :: [TrackData]
    , font1         :: Font
    , font2         :: Font
    }

-- generic resource handling
runArchive :: Archive a -> Zip.Archive -> StdGen -> a
runArchive m arc g = a
  where
    (materialMap, a) = flip evalState g $ flip runReaderT (materialMap, ar) $ liftM2 (,) loadMaterialMap m
    ar = T.fromList $ map (\e -> (SB.pack $ eFilePath e, decompress e)) arc

data Pattern
    = Opaque
    | Grate
    | Transparent
    | Grille
    | InverseGrille
    | Glass
    | InverseGlass

data Material
    = Material
    { pattern    :: Pattern
    , colorIndex :: Int
    , shininess  :: Float
    }

loadMaterialMap :: Archive (V.Vector Color, V.Vector Material)
loadMaterialMap = do
    unpackedCode <- extractDataFromPackedExecutable <$> executableImage
    Just paletteData <- (T.lookup "!pal") <$> loadRes "SDMAIN.PVS"
    let palette = V.fromList $ zipWith mkColor [0..] $ everyNth 3 $ LB.unpack $ LB.drop paletteOffset paletteData
    let materials = V.generate (fromIntegral materialCount) $ mkMaterial unpackedCode . fromIntegral
    return (palette, materials)

  where
    paletteOffset = 0x10
    materialCount = 129 :: Int64
    materialIndexOffset = 0xdbc8
    materialOpaqueInfoOffset = materialIndexOffset + materialCount * 2
    materialMaskOffset = materialIndexOffset + materialCount * 4
    opaqueMask = 0xffff

    mkColor :: Int -> [Word8] -> Color
    mkColor i [r, g, b] = Color (r `shiftL` 2) (g `shiftL` 2) (b `shiftL` 2) (if i == 255 then 0 else 0xff)

    mkMaterial unpackedCode i = Material
        { pattern    = if isOpaque then Opaque else (if i `elem` [22,23,24,34,94] then Grate else Transparent)
        , colorIndex = fromIntegral paletteIndex
        , shininess  = materialShininessMap V.! fromIntegral i
        }
      where
        paletteIndex = LB.index unpackedCode $ materialIndexOffset + i * 2
        isOpaque = LB.index unpackedCode (materialOpaqueInfoOffset + i * 2) == 0
        mask = if isOpaque then opaqueMask else readUshortAt $ materialMaskOffset + i * 2

        image = unpackedCode
        readUshortAt i = fromIntegral (LB.index image i) .|. (fromIntegral (LB.index image $ i+1) `shiftL` 8)

materialShininessMap :: V.Vector Float
materialShininessMap = V.fromList
    [  4.0,  4.0,  4.0,  4.0,  4.0,  4.0, 20.0,  4.0
    ,  4.0,  4.0,  4.0,  4.0,  4.0,  4.0,  4.0,  4.0
    ,  4.0,  0.0, 20.0, 20.0, 20.0, 20.0, 15.0, 15.0
    , 15.0,  4.0,  4.0,  4.0,  4.0,  4.0,  4.0,  4.0
    ,  4.0,  4.0,  4.0, 15.0, 15.0,  4.0, 20.0, 20.0
    , 40.0, 40.0, 40.0, 40.0, 40.0,  4.0,  4.0,  4.0
    , 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0
    , 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0
    , 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0
    , 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0
    , 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0
    , 30.0, 30.0, 30.0, 30.0,  4.0,  4.0,  4.0,  4.0
    ,  0.0,  0.0,  4.0,  4.0, 50.0,  4.0,  4.0,  4.0
    ,  4.0,  4.0,  4.0,  4.0,  4.0,  4.0, 30.0,  4.0
    ,  4.0, 20.0, 20.0, 20.0, 20.0, 20.0,  0.0,  4.0
    ,  4.0,  4.0,  4.0,  4.0,  4.0,  4.0,  4.0, 20.0
    , 20.0
    ]


everyNth :: Int -> [a] -> [[a]]
everyNth _ [] = []
everyNth n xs = take n xs: everyNth n (drop n xs)

{-
var importSettings = FindOrCreateAssetAtPath<ImportSettings>(ImportSettingsPath);
importSettings.SetDefaultMaterials();
-}

readStuntsData :: Zip.Archive -> StdGen -> StuntsData
readStuntsData = runArchive $ do
    cars <- mapM loadCar ["ANSX","COUN","JAGU","LM02","PC04","VETT","AUDI","FGTO","LANC","P962","PMIN"]
    keys <- asks $ T.keys . snd 
    tracks <- mapM loadTrack [k | k <- keys, ".trk" == takeExtensionCI (SB.unpack k)]
    Right font1 <- decodeFont <$> readZipFile "Prototype.ttf"
    Right font2 <- decodeFont <$> readZipFile "Capture_it.ttf"
    return $! StuntsData
        { cars    = cars
        , tracks  = tracks
        , font1   = font1
        , font2   = font2
        }

takeExtensionCI :: FilePath -> String
takeExtensionCI = map toLower . takeExtension
--isPrefixOfCI a b = isPrefixOf a $ map toLower b

type Archive = ReaderT ((V.Vector Color, V.Vector Material), T.Trie SB.ByteString) Rnd

type Rnd = State StdGen

rnd :: Random a => Rnd a
rnd = state random

getMaterialMap :: Archive (V.Vector Color, V.Vector Material)
getMaterialMap = asks fst

infix 7 !
(!) :: T.Trie a -> SB.ByteString -> a
m ! n = let Just d = T.lookup n m in d

readZipFile :: SB.ByteString -> Archive LB.ByteString
readZipFile = fmap (LB.fromChunks . (:[])) . readZipFile'

readZipFile' :: SB.ByteString -> Archive SB.ByteString
readZipFile' n = do
    x <- asks $ T.lookup n . snd
    return $ case x of
        Just d  -> d
        Nothing -> error $ "File not found: " ++ show n

-- constants
scaleFactor :: Float
scaleFactor = 0.3048 * 205 / 1024

edgeSize :: Float
edgeSize = 1024

hillHeight :: Float
hillHeight = 450

-- Graphics utility and conversion
wheelBase :: Int -> Model
wheelBase n = Model vl pl
  where
    fi = 2 * pi / fromIntegral n
    mkVertex :: Float -> Float -> Int -> (Float,Float,Float)
    mkVertex x r i = (x,r*sin j,r*cos j)
      where
        j = fi * fromIntegral i
    vl = [mkVertex z r i | r <- [1,0.55], z <- [-0.5,0.5], i <- [0..n-1]]
    pl = side ++ tread
    side = [Primitive (Polygon $ rev [n0..n0+n-1]) True (col == 40) [col] |
            (n0,rev,col) <- [(0,id,39),(n,reverse,39),(n*2,id,40),(n*3,reverse,40)]]
    tread = [Primitive (Polygon [i,i+n,i'+n,i']) True False [38] |
             i <- [0..n-1], let i' = if i == n-1 then 0 else i+1]

{-
data MeshAttribute
    = A_Float   (V.Vector Float)
    | A_V2F     (V.Vector V2F)
    | A_V3F     (V.Vector V3F)
    | A_V4F     (V.Vector V4F)
    | A_M22F    (V.Vector M22F)
    | A_M33F    (V.Vector M33F)
    | A_M44F    (V.Vector M44F)
    | A_Int     (V.Vector Int32)
    | A_Word    (V.Vector Word32)

data MeshPrimitive
    = P_Points
    | P_TriangleStrip
    | P_Triangles
    | P_TriangleStripI  (V.Vector Int)
    | P_TrianglesI      (V.Vector Int)

data Mesh
    = Mesh
    { mAttributes   :: T.Trie MeshAttribute
    , mPrimitive    :: MeshPrimitive
    , mGPUData      :: Maybe GPUData
    }
-}

{-
  mesh attributes:
    position
    normal
    ??
-}
{-
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
-}
-- TODO
toMesh :: (V.Vector Color, V.Vector Material) -> Model -> [Mesh]
toMesh (vgaPal, materialMap) mdOrig = [Mesh attrs p Nothing | p <- sml]
  where
    md = separateFaces . convertLines $ mdOrig

    groupToSubMesh ids = P_TrianglesI (vib triangulate)
      where
        --vsm pty = VSubMesh "StuntsMaterial" pty Nothing . Just
        vib fun = SV.fromList $ fun =<< ids
        triangulate (v0:vs@(_:_)) = concat [[toInt32 v0, toInt32 v1, toInt32 v2] | v1 <- tail vs | v2 <- vs]
        triangulate _ = []
        toInt32 :: Int -> Int32
        toInt32 = fromIntegral
    sml     = [groupToSubMesh [is | Primitive { prType = Polygon is } <- mdPrimitives md]]

    v3      = A_V3F . SV.convert . V.map (\(Vec3 x y z) -> V3 x y z)
    v4      = A_V4F . SV.convert . V.map (\(Vec4 x y z w) -> V4 x y z w)
    i32     = A_Int . SV.convert
    f32     = A_Float . SV.convert
    f       = realToFrac
    v       = V.fromList [Vec3 (f x) (f y) (f z) | (x,y,z) <- mdVertices md]
    attrs   = if V.length n == V.length v
                then T.fromList [("position",v3 v), ("normal", v3 n), ("colour", v4 c), ("pattern", i32 p), ("zBias", f32 z), ("shininess", f32 s)]
                else error $ "not matching sizes: " ++ show (V.length n) ++ " " ++ show (V.length v)

    genNormal (a:b:c:_) = normalize $ (vc &- va) &^ (vb &- va)
      where
        va = v V.! a
        vb = v V.! b
        vc = v V.! c
    genNormal _ = zero

    genMat pr = (Vec4 (f r) (f g) (f b) 1, p, z, shiny)
      where
        i = (cycle $ prMaterials pr) !! 1
        Material pattern ci shiny = materialMap V.! i
        Color r g b _ = vgaPal V.! ci
        f i = fromIntegral i / 255
        p = case pattern of
            Grate        -> 2
            Transparent  -> 0
            _            -> 1
        z = if i `elem` [16,101,102,103,104,105] then 1 else if prZBias pr then -1 else 0

    (nf,cf,pf,zf,sf) = V.unzip5 $ V.fromList [(genNormal (prIndices $ prType pr), c, p, z, s) | pr <- mdPrimitives md, let (c,p,z,s) = genMat pr]
    i = V.fromList [ix | (ix,pr) <- zip [0..] (mdPrimitives md), _ <- prIndices $ prType pr]
    n = V.backpermute nf i
    c = V.backpermute cf i
    p = V.backpermute pf i
    z = V.backpermute zf i
    s = V.backpermute sf i

prIndices (Particle v) = [v]
prIndices (Line a b) = [a,b]
prIndices (Polygon is) = is
prIndices (Sphere a b) = [a,b]
prIndices (Wheel a b c d e f) = a:b:c:d:e:f:[]
prIndices Ignored = []
prIndices _ = error "prIdices"

separateFaces :: Model -> Model
separateFaces md = Model { mdVertices = vs', mdPrimitives = prs' }
  where
    vs = V.fromList (mdVertices md)
    vs' = [vs V.! ix | pr <- mdPrimitives md, ix <- prIndices $ prType pr]
    prs' = go 0 (mdPrimitives md)
      where
        go _ [] = []
        go n (pr:prs) = n' `seq` pr' : go n' prs
          where
            l = length (prIndices $ prType pr)
            n' = n+l
            pr' = pr { prType = ff (prType pr) $ take l [n..] }

            ff (Particle _) (a:_) = Particle a
            ff (Line _ _) (a:b:_) = Line a b
            ff (Polygon xs) l = Polygon $ take (length xs) l
            ff (Sphere _ _) (a:b:_) = Sphere a b
            ff (Wheel _ _ _ _ _ _) (a:b:c:d:e:f:_) = Wheel a b c d e f
            ff Ignored _ = Ignored

convertLines :: Model -> Model
convertLines md = Model { mdVertices = mdVertices md ++ concat vs', mdPrimitives = notLines ++ concat prs' }
  where
    vs = V.fromList (mdVertices md)
    notLines = filter (notLine . prType) (mdPrimitives md)
    notLine (Line _ _) = False
    notLine _ = True
    nvs = V.length vs
    (vs',prs') = unzip [mkLine i0 i1 i2 pr | i0 <- [nvs,nvs+n*2..] | pr@(Primitive { prType = Line i1 i2 }) <- mdPrimitives md]
    mkLine i0 i1 i2 pr = (map (\(Vec3 x y z) -> (x,y,z)) vs', end ++ side)
      where
        (x1,y1,z1) = vs V.! i1
        (x2,y2,z2) = vs V.! i2
        v1 = Vec3 x1 y1 z1
        v2 = Vec3 x2 y2 z2
        vd = v1 &- v2
        vd' = Vec3 1 1 1
        vdp = normalize (vd &^ vd') &* r
        vdp' = normalize (vd &^ vdp) &* r
        r = 0.007 * len vd
        vs' = [v &+ (s *& vdp) &+ (c *& vdp') | v <- [v1,v2], (s,c) <- scs]
        end = [mkp [i0..i0+n'], mkp [i0+n..i0+n+n']]
        side = [mkp [i0+i,i0+i+n,i0+i'+n,i0+i'] | i <- [0..n'], let i' = if i == n' then 0 else i+1]
        mkp is = pr { prType = Polygon is, prZBias = False }

    n = 8
    n' = n-1
    scs = [(sin (i*2*pi/n), cos (i*2*pi/n)) | i <- [0..n']]

-- utility
groupSetBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSetBy f = groupBy (\x y -> f x y == EQ) . sortBy f

toProj4 :: Float -> Int -> Int -> Bool -> Proj4
toProj4 o x y e = (orthogonal $ rightOrthoU $ toU o) .*. translation (toVec3 x y e) .*. scalingUniformProj4 scaleFactor

toVec3 :: Int -> Int -> Bool -> Vec3
toVec3 x y e = Vec3 (edgeSize * fromIntegral x) (if e then hillHeight else 0) (edgeSize * fromIntegral y)

toU :: Float -> U
toU o = rotU (Vec3 0 1 0) $ realToFrac o
