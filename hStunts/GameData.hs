{-# LANGUAGE NoMonomorphismRestriction, ParallelListComp, OverloadedStrings #-}

module GameData where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Binary.Get as B
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Ord
import Data.Vect.Float
import Data.Vect.Float.Instances
import Data.Vect.Float.Util.Quaternion hiding (toU)
import System.IO.Unsafe
import System.Random
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.IntMap as IM
import qualified Data.Trie as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import System.Directory
import System.FilePath

import LC_API hiding (Line)
import LC_Mesh hiding (loadMesh)
import Stunts.Color
import Stunts.Loader
import Stunts.Track
import Stunts.Unpack
import Zip

import GameGraphics
import MeshUtil

bitmaps :: [(SB.ByteString,Bitmap)]
bitmaps = concat [T.toList $ loadBitmap k | k <- T.keys archive, ".pvs" == takeExtensionCI (SB.unpack k)]

-- game specific resource handling
loadRes :: SB.ByteString -> T.Trie LB.ByteString
loadRes = readResources . unpackResource . readZipFile

loadMesh :: SB.ByteString -> T.Trie [Mesh]
loadMesh = fmap (toMesh . runGet getModel) . loadRes

loadBitmap :: SB.ByteString -> T.Trie Bitmap
loadBitmap = fmap (runGet getBitmap) . loadRes

loadCarMesh :: SB.ByteString -> T.Trie [Mesh]
loadCarMesh n = T.mapBy (\k -> Just . toMesh . fixOp k . runGet getModel) $! loadRes n
  where
    fixOp k = if k == "car0" then addBottom . fixLambo else id

    -- Remove stray faces from the bottom of the Lamborghini model
    fixLambo md = if n /= "STCOUN.P3S" then md else md'
      where
        miny = minimum [y | (_,y,_) <- mdVertices md]
        ixs = findIndices (\(_,y,_) -> y == miny) (mdVertices md)
        md' = md { mdPrimitives =
                        [pr | pr <- mdPrimitives md,
                         prType pr /= Polygon || null (intersect ixs (prIndices pr))]
                 }

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

        edges = [e | Primitive { prType = Polygon, prIndices = ixs } <- mdPrimitives md,
                 all ((0 <=) . _1 . vec) ixs, e <- zip ixs (last ixs : ixs)]

        uniqueEdges = go edges
          where
            go [] = []
            go ((i1,i2):es) = case findIndex sameEdge es of
                Just _  -> go (filter (not . sameEdge) es)
                Nothing -> (i1,i2) : go es
              where
                sameEdge (i1',i2') = (i1,i2) == (i1',i2') || (i2,i1) == (i1',i2')

        newFaces = [Primitive Polygon False False [57] ixs |
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
            ps = [plane i1 i2 i3 | Primitive { prType = Polygon, prIndices = i1:i2:i3:_ } <- mdPrimitives md]
            plane i1 i2 i3 = (normalize ((v2 &- v1) &^ (v3 &- v1)), v1)
              where
                v1 = vec i1
                v2 = vec i2
                v3 = vec i3
        isNewFace _ = False

loadCarWheels :: SB.ByteString -> [(Vec3, Float, Float)]
loadCarWheels n = [wheel vl $ prIndices p | let Model vl pl = m ! "car0", p <- pl, prType p == Wheel]
  where
    m = fmap (runGet getModel) $ loadRes n
    -- wheel pos, wheel width, wheel radius
    wheel vl [p1,p2,p3,p4,p5,p6] = ((v p1 + v p4) &* (0.5 * car0ScaleFactor),car0ScaleFactor * (len $ v p1 - v p4),car0ScaleFactor * (len $ v p2 - v p1))
      where
        v i = let (a,b,c) = vl !! i in Vec3 a b c
    car0ScaleFactor = scaleFactor / 20

loadCar :: String -> (T.Trie [Mesh], Car, [(Vec3, Float, Float)], T.Trie Bitmap)
loadCar n = (mesh,runGet getCar $ carRes ! "simd",wheels,bitmaps)
  where
    mesh = loadCarMesh $ SB.pack $ "ST" ++ n ++ ".P3S"
    carRes = loadRes $ SB.pack $ "CAR" ++ n ++ ".RES"
    wheels = loadCarWheels $ SB.pack $ "ST" ++ n ++ ".P3S"
    bitmaps = T.unionL bitmapsA bitmapsB
    bitmapsA = loadBitmap $ SB.pack $ "STDA" ++ n ++ ".PVS"
    bitmapsB = loadBitmap $ SB.pack $ "STDB" ++ n ++ ".PVS"

loadTrack :: SB.ByteString -> IO ([(Proj4, [Mesh])], [(Proj4, [Mesh])], (Float, Vec3))
loadTrack trkFile = do
    let game1Map = loadMesh "GAME1.P3S"
        game2Map = loadMesh "GAME2.P3S"
        (terrainItems,trackItems) = readTrack $ readZipFile' trkFile

        modelIdToMesh :: (SB.ByteString,SB.ByteString) -> [Mesh]
        modelIdToMesh ("GAME1.P3S",n) = game1Map ! n
        modelIdToMesh ("GAME2.P3S",n) = game2Map ! n
        modelIdToMesh (n,_) = error $ "Unknown resource file: " ++ show n
        f (a,b,c) = (map modelIdToMesh a, map modelIdToMesh b,c)

        terrainMap  = IM.map f terrainModelMap
        trackMap    = IM.mapWithKey (\k (a,b,c) -> (map (map (clampItem k)) a, map (map (clampItem k)) b, c)) (IM.map f trackModelMap)

        clampItem :: Int -> Mesh -> Mesh
        clampItem i (Mesh attrs prim Nothing) = Mesh (T.mapBy clamp attrs) prim Nothing
          where
            (iw,ih) = trackModelSizeMap IM.! i
            (ms,_,_) = trackModelMap IM.! i
            clamp n (A_V3F a)
                | n == "position" = Just $ A_V3F $ if iw == 1 && ih == 1 then go a else a
                | otherwise = Just $ A_V3F a
              where
                minmax = min 512 . max (-512)
                go v = SV.map (\(V3 x y z) -> V3 (minmax x) y (minmax z)) v
            clamp _ a = Just a

        terrain = [(toProj4 o x y e,m) | (i,x,y,e) <- terrainItems, let (ml,_,o) = terrainMap IM.! i, m <- ml] -- U Vec3 Mesh
        track   = [(toProj4' o i x y e,m) | (i,x,y,e) <- trackItems, let (ml,_,o) = trackMap IM.! i, m <- ml] -- U Vec3 Mesh

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

    clouds <- replicateM 70 $ do
        let getCloudMesh n = game2Map ! (SB.pack $ "cld" ++ show (1 + n `mod` 3 :: Int))
            getCoord a d = (a', c sin, c cos)
              where
                a' = a*2*pi
                c t = ((50+200*d)*t a'+15)*edgeSize*scaleFactor
        m <- getCloudMesh <$> randomIO
        (a,x,z) <- getCoord <$> randomIO <*> randomIO
        y <- randomIO
        return (scalingUniformProj4 (y*0.4+0.3) .*. rotMatrixProj4 a (Vec3 0 1 0) .*. translation (Vec3 x (y*1500+600) z), m)
    return (terrain ++ clouds ++ fence, track,startPos)

data CarData
    = CarData
    { carMesh       :: [Mesh]
    , wheels        :: [(Vec3,Float,Float,[Mesh])]
    , carSimModel   :: Car
    , carBitmaps    :: T.Trie Bitmap
    }

data StuntsData
    = StuntsData
    { cars          :: [CarData]
    , terrainMesh   :: [Mesh]
    , trackMesh     :: [Mesh]
    , startPosition :: (Float,Vec3)
    }

readStuntsData :: Int -> SB.ByteString -> IO StuntsData
readStuntsData carNum trkFile = do
    (terrain,track,startPos) <- loadTrack trkFile
    let cars = map (mkCarData . loadCar) ["ANSX","COUN","JAGU","LM02","PC04","VETT","AUDI","FGTO","LANC","P962","PMIN"]
        mkCarData (carModel,carSim,carWheels,carBmps) =
            CarData
            { carMesh     = [transformMesh' (scalingUniformProj4 (1/20) .*. toProj4 pi 0 0 False) m | m <- carModel ! "car0"]
            , wheels      = [(p,w,r,map (transformMesh' (scaling $ Vec3 w r r)) (toMesh (wheelBase 16))) | (p,w,r) <- carWheels]
            , carSimModel = carSim
            , carBitmaps  = carBmps
            }
    return $! StuntsData
        { cars          = cars
        , terrainMesh   = [transformMesh' p m | (p,ml) <- terrain, m <- ml]
        , trackMesh     = [transformMesh' p m | (p,ml) <- track, m <- ml]
        , startPosition = startPos
        }

takeExtensionCI = map toLower . takeExtension
isPrefixOfCI a b = isPrefixOf a $ map toLower b

-- generic resource handling
archive :: T.Trie SB.ByteString
{-# NOINLINE archive #-} 
archive = unsafePerformIO $! do
    a <- concat <$> (mapM readArchive =<< filter (\n -> ".zip" == takeExtensionCI n) <$> getDirectoryContents ".")
    let ar = T.fromList $ map (\e -> (SB.pack $ eFilePath e, decompress e)) a
    print $ T.keys ar
    return ar

infix 7 !
(!) :: T.Trie a -> SB.ByteString -> a
m ! n = let Just d = T.lookup n m in d

readZipFile :: SB.ByteString -> LB.ByteString
readZipFile n = case T.lookup n archive of
    Just d  -> LB.fromChunks [d]
    Nothing -> error $ "File not found: " ++ show n

readZipFile' :: SB.ByteString -> SB.ByteString
readZipFile' n = case T.lookup n archive of
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
    side = [Primitive Polygon True (col == 40) [col] (rev [n0..n0+n-1]) |
            (n0,rev,col) <- [(0,id,39),(n,reverse,39),(n*2,id,40),(n*3,reverse,40)]]
    tread = [Primitive Polygon True False [38] [i,i+n,i'+n,i'] |
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
toMesh :: Model -> [Mesh]
toMesh mdOrig = [Mesh attrs p Nothing | p <- sml]
  where
    md = separateFaces . convertLines $ mdOrig

    groupToSubMesh prs@(pr:_) = case prType pr of
        --Particle    -> vsm OT_POINT_LIST (vib id)
        --Line        -> vsm OT_LINE_LIST (vib id)
        Polygon     -> P_TrianglesI (vib triangulate)
        _           -> P_TrianglesI SV.empty
      where
        --vsm pty = VSubMesh "StuntsMaterial" pty Nothing . Just
        vib fun = SV.fromList $ fun . prIndices =<< prs
        triangulate (v0:vs@(_:_)) = concat [[toInt32 v0, toInt32 v1, toInt32 v2] | v1 <- tail vs | v2 <- vs]
        triangulate _ = []
        toInt32 :: Int -> Int32
        toInt32 = fromIntegral
    sml     = map groupToSubMesh $ groupSetBy (comparing (prMaterials &&& prType)) $ mdPrimitives md

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

    genMat pr = (Vec4 r g b 1, p, z, shiny)
      where
        i = (cycle $ prMaterials pr) !! 1
        Material pattern rgb _ shiny = materialMap IM.! i
        r = fromIntegral (rgb `shiftR` 16) / 255
        g = fromIntegral ((rgb `shiftR` 8) .&. 0xff) / 255
        b = fromIntegral (rgb .&. 0xff) / 255
        p = case pattern of
            Grate        -> 2
            Transparent  -> 0
            _            -> 1
        z = if i `elem` [16,101,102,103,104,105] then 1 else if prZBias pr then -1 else 0

    (nf,cf,pf,zf,sf) = V.unzip5 $ V.fromList [(genNormal (prIndices pr), c, p, z, s) | pr <- mdPrimitives md, let (c,p,z,s) = genMat pr]
    i = V.fromList [ix | (ix,pr) <- zip [0..] (mdPrimitives md), _ <- prIndices pr]
    n = V.backpermute nf i
    c = V.backpermute cf i
    p = V.backpermute pf i
    z = V.backpermute zf i
    s = V.backpermute sf i

separateFaces :: Model -> Model
separateFaces md = Model { mdVertices = vs', mdPrimitives = prs' }
  where
    vs = V.fromList (mdVertices md)
    vs' = [vs V.! ix | pr <- mdPrimitives md, ix <- prIndices pr]
    prs' = go 0 (mdPrimitives md)
      where
        go _ [] = []
        go n (pr:prs) = n' `seq` pr' : go n' prs
          where
            l = length (prIndices pr)
            n' = n+l
            pr' = pr { prIndices = take l [n..] }

convertLines :: Model -> Model
convertLines md = Model { mdVertices = mdVertices md ++ concat vs', mdPrimitives = notLines ++ concat prs' }
  where
    vs = V.fromList (mdVertices md)
    (lines,notLines) = partition ((==Line) . prType) (mdPrimitives md)
    nvs = V.length vs
    (vs',prs') = unzip [mkLine i0 pr | i0 <- [nvs,nvs+n*2..] | pr <- lines]
    mkLine i0 pr = (map (\(Vec3 x y z) -> (x,y,z)) vs', end ++ side)
      where
        (x1,y1,z1):(x2,y2,z2):_ = map (vs V.!) (prIndices pr)
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
        mkp is = pr { prType = Polygon, prZBias = False, prIndices = is }

    n = 8
    n' = n-1
    scs = [(sin (i*2*pi/n), cos (i*2*pi/n)) | i <- [0..n']]

-- utility
groupSetBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSetBy f = groupBy (\x y -> f x y == EQ) . sortBy f

toProj4 :: Float -> Int -> Int -> Bool -> Proj4
toProj4 o x y e = (orthogonal $ rightOrthoU $ toU o) .*. translation (toVec3 x y e) .*. scalingUniformProj4 scaleFactor

toProj4' :: Float -> Int -> Int -> Int -> Bool -> Proj4
toProj4' o i x y e = (orthogonal $ rightOrthoU $ toU o) .*. translation (toVec3' i x y e) .*. scalingUniformProj4 scaleFactor

toVec3 :: Int -> Int -> Bool -> Vec3
toVec3 x y e = Vec3 (edgeSize * fromIntegral x) (if e then hillHeight else 0) (edgeSize * fromIntegral y)

toVec3' :: IM.Key -> Int -> Int -> Bool -> Vec3
toVec3' i x y e = Vec3 (edgeSize * x') (if e then hillHeight else 0) (edgeSize * y')
  where
    f = fromIntegral :: Int -> Float
    (iw,ih) = trackModelSizeMap IM.! i
    x' = f x + (f iw - 1) * 0.5
    y' = f y + (f ih - 1) * 0.5

toU :: Float -> U
toU o = rotU (Vec3 0 1 0) $ realToFrac o
