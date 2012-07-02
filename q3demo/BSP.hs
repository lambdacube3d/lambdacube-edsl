{-# LANGUAGE OverloadedStrings #-}
module BSP where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.Int
import Data.Word

import Data.Binary as B
import Data.Binary.Get as B
import Data.Binary.IEEE754
import Data.Vect hiding (Vector)
import Data.Vector (Vector)
import qualified Data.ByteString as SB8
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V

{-
Information: http://graphics.stanford.edu/~kekoa/q3/

Data types

Quake 3 BSP files contains only four basic data types. They are:

Type        Description
ubyte       unsigned byte
int         4-byte integer, little-endian
float       4-byte IEEE float, little-endian
string[n]   string of n ASCII bytes, not necessarily null-terminated

All data in a BSP file is organized into records composed of these four data types.
-}

-- http://www.mralligator.com/q3/

lumpEntities     =  0 :: Int -- ^ Game-related object descriptions
lumpShaders      =  1 :: Int -- ^ Stores texture information
lumpPlanes       =  2 :: Int -- ^ Stores the splitting planes
lumpNodes        =  3 :: Int -- ^ Stores the BSP nodes
lumpLeaves       =  4 :: Int -- ^ Stores the leafs of the nodes
lumpLeafSurfaces =  5 :: Int -- ^ Stores the leaf's indices into the faces
lumpLeafBrushes  =  6 :: Int -- ^ Stores the leaf's indices into the brushes
lumpModels       =  7 :: Int -- ^ Descriptions of rigid world geometry in map
lumpBrushes      =  8 :: Int -- ^ Stores the brushes info (for collision)
lumpBrushSides   =  9 :: Int -- ^ Stores the brush surfaces
lumpDrawVertices = 10 :: Int -- ^ Stores the level vertices
lumpDrawIndices  = 11 :: Int -- ^ Stores the level indices
lumpFogs         = 12 :: Int -- ^ List of special map effects
lumpSurfaces     = 13 :: Int -- ^ Stores the faces for the level
lumpLightmaps    = 14 :: Int -- ^ Stores the lightmaps for the level
lumpLightGrid    = 15 :: Int -- ^ Local illumination data
lumpVisibility   = 16 :: Int -- ^ Stores PVS and cluster info (visibility)

data Model
    = Model
    { mdMins         :: !Vec3
    , mdMaxs         :: !Vec3
    , mdFirstSurface :: !Int
    , mdNumSurfaces  :: !Int
    , mdFirstBrush   :: !Int
    , mdNumBrushes   :: !Int
    }

data Shader
    = Shader
    { shName         :: !SB.ByteString
    , shSurfaceFlags :: !Int
    , shContentFlags :: !Int
    }

data Plane
    = Plane
    { plNormal :: !Vec3
    , plDist   :: !Float
    }

data Node
    = Node
    { ndPlaneNum :: !Int
    , ndChildren :: !(Int,Int)
    , ndMins     :: !Vec3
    , ndMaxs     :: !Vec3
    }

data Leaf
    = Leaf
    { lfCluster          :: !Int
    , lfArea             :: !Int
    , lfMins             :: !Vec3
    , lfMaxs             :: !Vec3
    , lfFirstLeafSurface :: !Int
    , lfNumLeafSurfaces  :: !Int
    , lfFirstLeafBrush   :: !Int
    , lfNumLeafBrushes   :: !Int
    }

data BrushSide
    = BrushSide
    { bsPlaneNum  :: !Int
    , bsShaderNum :: !Int
    }

data Brush
    = Brush
    { brFirstSide :: !Int
    , brNumSides  :: !Int
    , brShaderNum :: !Int
    }

data Fog
    = Fog
    { fgName        :: !SB.ByteString
    , fgBrushNum    :: !Int
    , fgVisibleSide :: !Int
    }

data DrawVertex
    = DrawVertex
    { dvPosition    :: !Vec3
    , dvDiffuseUV   :: !Vec2
    , dvLightmaptUV :: !Vec2
    , dvNormal      :: !Vec3
    , dvColor       :: !Vec4
    }

data SurfaceType
    = Planar
    | Patch
    | TriangleSoup
    | Flare

data Surface
    = Surface
    { srShaderNum      :: !Int
    , srFogNum         :: !Int
    , srSurfaceType    :: !SurfaceType
    , srFirstVertex    :: !Int
    , srNumVertices    :: !Int
    , srFirstIndex     :: !Int
    , srNumIndices     :: !Int
    , srLightmapNum    :: !Int
    , srLightmapPos    :: !Vec2
    , srLightmapSize   :: !Vec2
    , srLightmapOrigin :: !Vec3
    , srLightmapVec1   :: !Vec3
    , srLightmapVec2   :: !Vec3
    , srLightmapVec3   :: !Vec3
    , srPatchSize      :: !(Int,Int)
    }

data Lightmap
    = Lightmap
    { lmMap :: !SB.ByteString
    }

data LightGrid
    = LightGrid

data Visibility
    = Visibility
    { vsNumVecs     :: !Int
    , vsSizeVecs    :: !Int
    , vsVecs        :: !(Vector Word8)
    }

data BSPLevel
    = BSPLevel
    { blEntities     :: !SB.ByteString
    , blShaders      :: !(Vector Shader)
    , blPlanes       :: !(Vector Plane)
    , blNodes        :: !(Vector Node)
    , blLeaves       :: !(Vector Leaf)
    , blLeafSurfaces :: !(Vector Int)
    , blLeafBrushes  :: !(Vector Int)
    , blModels       :: !(Vector Model)
    , blBrushes      :: !(Vector Brush)
    , blBrushSides   :: !(Vector BrushSide)
    , blDrawVertices :: !(Vector DrawVertex)
    , blDrawIndices  :: !(Vector Int)
    , blFogs         :: !(Vector Fog)
    , blSurfaces     :: !(Vector Surface)
    , blLightmaps    :: !(Vector Lightmap)
    , blLightgrid    :: !(Vector LightGrid)
    , blVisibility   :: !Visibility
    }

getString   = fmap (SB.takeWhile (/= '\0')) . getByteString

getWord     = getWord32le

getUByte    = B.get :: Get Word8
getUByte2   = B.get :: Get (Word8,Word8)
getUByte3   = B.get :: Get (Word8,Word8,Word8)

getFloat    = getFloat32le

getVec2     = Vec2 <$> getFloat <*> getFloat
getVec3     = (\x y z -> Vec3 x z (-y)) <$> getFloat <*> getFloat <*> getFloat
getVec2i    = (\x y -> Vec2 (fromIntegral x) (fromIntegral y)) <$> getInt <*> getInt
getVec3i    = (\x y z -> Vec3 (fromIntegral x) (fromIntegral z) (fromIntegral (-y))) <$> getInt <*> getInt <*> getInt

getVec4RGBA = (\r g b a -> Vec4 (f r) (f g) (f b) (f a)) <$> getUByte <*> getUByte <*> getUByte <*> getUByte
  where
    f v = fromIntegral v / 255

getInt      = fromIntegral <$> getInt' :: Get Int
  where
    getInt' = fromIntegral <$> getWord32le :: Get Int32

getInt2     = (,) <$> getInt <*> getInt

getItems elemSize a byteCount = V.replicateM (byteCount `div` elemSize) a

getHeader = do
    magic <- getString 4
    case magic == "IBSP" of
        True    -> return ()
        _       -> fail "Invalid format."
    version <- getWord
    replicateM 17 getInt2

getSurfaceType  = getInt >>= \i -> case i of
    1 -> return Planar
    2 -> return Patch
    3 -> return TriangleSoup
    4 -> return Flare
    _ -> fail "Invalid surface type"

getEntities l   = getString l
getShaders      = getItems  72 $ Shader     <$> (SB.map toLower <$> getString 64) <*> getInt <*> getInt
getPlanes       = getItems  16 $ Plane      <$> getVec3 <*> getFloat
getNodes        = getItems  36 $ Node       <$> getInt <*> getInt2 <*> getVec3i <*> getVec3i
getLeaves       = getItems  48 $ Leaf       <$> getInt <*> getInt <*> getVec3i <*> getVec3i <*> getInt <*> getInt <*> getInt <*> getInt
getLeafSurfaces = getItems   4   getInt
getLeafBrushes  = getItems   4   getInt
getModels       = getItems  40 $ Model      <$> getVec3 <*> getVec3 <*> getInt <*> getInt <*> getInt <*> getInt
getBrushes      = getItems  12 $ Brush      <$> getInt <*> getInt <*> getInt
getBrushSides   = getItems   8 $ BrushSide  <$> getInt <*> getInt
getDrawVertices = getItems  44 $ DrawVertex <$> getVec3 <*> getVec2 <*> getVec2 <*> getVec3 <*> getVec4RGBA
getDrawIndices  = getItems   4   getInt
getFogs         = getItems  72 $ Fog        <$> getString 64 <*> getInt <*> getInt
getSurfaces     = getItems 104 $ Surface    <$> getInt <*> getInt <*> getSurfaceType <*> getInt <*> getInt <*> getInt <*> getInt <*> getInt
                                            <*> getVec2i <*> getVec2i <*> getVec3 <*> getVec3 <*> getVec3 <*> getVec3 <*> getInt2
getLightmaps    = getItems (128*128*3) (Lightmap <$> (getByteString $ 128*128*3))

getLightGrid = getItems 8 $ do
    ambient     <- getUByte3
    directional <- getUByte3
    dir         <- getUByte2
    return LightGrid

getVisibility l = do
    nvecs   <- getInt
    szvecs  <- getInt
    vecs    <- getByteString $ nvecs * szvecs
    return $ Visibility nvecs szvecs $ V.fromList $ SB8.unpack vecs

readBSP :: LB.ByteString -> BSPLevel
readBSP dat = BSPLevel
    (lump getEntities      lumpEntities)
    (lump getShaders       lumpShaders)
    (lump getPlanes        lumpPlanes)
    (lump getNodes         lumpNodes)
    (lump getLeaves        lumpLeaves)
    (lump getLeafSurfaces  lumpLeafSurfaces)
    (lump getLeafBrushes   lumpLeafBrushes)
    (lump getModels        lumpModels)
    (lump getBrushes       lumpBrushes)
    (lump getBrushSides    lumpBrushSides)
    (lump getDrawVertices  lumpDrawVertices)
    (lump getDrawIndices   lumpDrawIndices)
    (lump getFogs          lumpFogs)
    (lump getSurfaces      lumpSurfaces)
    (lump getLightmaps     lumpLightmaps)
    (lump getLightGrid     lumpLightGrid)
    (lump getVisibility    lumpVisibility)
  where
    el = runGet getHeader dat
    lump g i = runGet (let (o,l) = el !! i in skip o >> g l) dat

loadBSP :: String -> IO BSPLevel
loadBSP n = readBSP <$> LB.readFile n
