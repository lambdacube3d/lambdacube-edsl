{-# LANGUAGE OverloadedStrings #-}
module MD3 where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Word

import Data.Binary as B
import Data.Binary.Get as B
import Data.Binary.IEEE754
import Data.Vect hiding (Vector)
import Data.Vector (Vector)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V

data Frame
    = Frame
    { frMins    :: !Vec3
    , frMaxs    :: !Vec3
    , frOrigin  :: !Vec3
    , frRadius  :: !Float
    , frName    :: !SB.ByteString
    } deriving Show

data Tag
    = Tag
    { tgName    :: !SB.ByteString
    , tgOrigin  :: !Vec3
    , tgAxisX   :: !Vec3
    , tgAxisY   :: !Vec3
    , tgAxisZ   :: !Vec3
    } deriving Show

data Shader
    = Shader
    { shName    :: !SB.ByteString
    , shIndex   :: !Int
    } deriving Show

data Surface
    = Surface
    { srName        :: SB.ByteString
    , srShaders     :: Vector Shader
    , srTriangles   :: Vector (Int,Int,Int)
    , srTexCoords   :: Vector Vec2
    , srXyzNormal   :: Vector (Vector (Vec3,Vec3))
    } deriving Show

data MD3Model
    = MD3Model
    { mdFrames      :: Vector Frame
    , mdTags        :: Vector (Vector Tag)
    , msSurfaces    :: Vector Surface
    } deriving Show

getString   = fmap (SB.takeWhile (/= '\0')) . getByteString
getUByte    = B.get :: Get Word8
getFloat    = getFloat32le
getVec2     = Vec2 <$> getFloat <*> getFloat
getVec3     = (\x y z -> Vec3 x z (-y)) <$> getFloat <*> getFloat <*> getFloat
getVec3i16  = (\x y z -> Vec3 (fromIntegral x) (fromIntegral z) (fromIntegral (-y))) <$> getInt16 <*> getInt16 <*> getInt16
getInt16    = fromIntegral <$> getInt' :: Get Int
  where
    getInt' = fromIntegral <$> getWord16le :: Get Int16
getInt      = fromIntegral <$> getInt' :: Get Int
  where
    getInt' = fromIntegral <$> getWord32le :: Get Int32
getInt3     = (,,) <$> getInt <*> getInt <*> getInt
getAngle    = (\i -> fromIntegral i * 2 * pi / 255) <$> getUByte
getV o n f dat              = runGet (V.replicateM n f) (LB.drop (fromIntegral o) dat)
getVV o nOuter nInner f dat = V.generate nOuter (\i -> V.take nInner $ V.drop (nInner * i) $ getV o (nOuter*nInner) f dat)

getFrame    = Frame <$> getVec3 <*> getVec3 <*> getVec3 <*> getFloat <*> getString 64
getTag      = Tag <$> getString 64 <*> getVec3 <*> getVec3 <*> getVec3 <*> getVec3
getShader   = Shader <$> getString 64 <*> getInt

getXyzNormal = do
    v <- getVec3i16
    lat <- getAngle
    lng <- getAngle
    return (v &* 64, Vec3 (cos lng * sin lat) (cos lat) (-sin lng * sin lat))

getSurface = (\(o,v) -> skip o >> return v) =<< lookAhead getSurface'
  where
    getSurface' = do
        dat <- getRemainingLazyByteString
        "IDP3" <- getString 4
        name <- getString 64
        flags <- getInt
        [nFrames,nShaders,nVerts,nTris] <- replicateM 4 getInt
        [oTris,oShaders,oTexCoords,oXyzNormals,oEnd] <- replicateM 5 getInt
        return $ (oEnd,Surface name (getV oShaders nShaders getShader dat) (getV oTris nTris getInt3 dat)
                                    (getV oTexCoords nVerts getVec2 dat) (getVV oXyzNormals nFrames nVerts getXyzNormal dat))

getMD3Model = do
    dat <- getRemainingLazyByteString
    "IDP3" <- getString 4
    version <- getInt
    name <- getString 64
    flags <- getInt
    [nFrames,nTags,nSurfaces,nSkins] <- replicateM 4 getInt
    [oFrames,oTags,oSurfaces,oSkins,oEnd] <- replicateM 5 getInt
    return $ MD3Model (getV oFrames nFrames getFrame dat) (getVV oTags nFrames nTags getTag dat) (getV oSurfaces nSurfaces getSurface dat)

loadMD3 :: String -> IO MD3Model
loadMD3 n = readMD3 <$> LB.readFile n

readMD3 dat = runGet getMD3Model dat
