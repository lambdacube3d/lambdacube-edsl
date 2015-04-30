{-# LANGUAGE TupleSections #-}
module Backend.GL.Mesh (
    loadMesh,
    saveMesh,
    addMesh,
    compileMesh,
    updateMesh,
    Mesh(..),
    MeshPrimitive(..),
    MeshAttribute(..)
) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.ByteString.Char8 (ByteString)
import Foreign.Ptr
import Data.Int
import Foreign.Storable
import Foreign.Marshal.Utils
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Trie as T
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Backend.GL
import Backend.GL.Type as T
import IR as IR

fileVersion :: Int32
fileVersion = 1

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
    | P_TriangleStripI  (V.Vector Int32)
    | P_TrianglesI      (V.Vector Int32)

data Mesh
    = Mesh 
    { mAttributes   :: T.Trie MeshAttribute
    , mPrimitive    :: MeshPrimitive
    , mGPUData      :: Maybe GPUData
    }

data GPUData
    = GPUData
    { dPrimitive    :: Primitive
    , dStreams      :: T.Trie (Stream Buffer)
    , dIndices      :: Maybe (IndexStream Buffer)
    }

loadMesh :: String -> IO Mesh
loadMesh n = compileMesh =<< decode <$> LB.readFile n

saveMesh :: String -> Mesh -> IO ()
saveMesh n m = LB.writeFile n (encode m)

addMesh :: GLPipelineInput -> ByteString -> Mesh -> [ByteString] -> IO Object
addMesh input slotName (Mesh _ _ (Just (GPUData prim streams indices))) objUniNames = do
    -- select proper attributes
    let Just (SlotSchema slotPrim slotStreams) = T.lookup slotName $! T.slots $! T.schema input
        filterStream n s
            | T.member n slotStreams = Just s
            | otherwise = Nothing
    addObject input slotName prim indices (T.mapBy filterStream streams) objUniNames
addMesh _ _ _ _ = fail "addMesh: only compiled mesh with GPUData is supported"

withV w a f = w a (\p -> f $ castPtr p)

meshAttrToArray :: MeshAttribute -> Array
meshAttrToArray (A_Float v) = Array ArrFloat  (1 *  V.length v) $ withV V.unsafeWith v
meshAttrToArray (A_V2F   v) = Array ArrFloat  (2 *  V.length v) $ withV V.unsafeWith v
meshAttrToArray (A_V3F   v) = Array ArrFloat  (3 *  V.length v) $ withV V.unsafeWith v
meshAttrToArray (A_V4F   v) = Array ArrFloat  (4 *  V.length v) $ withV V.unsafeWith v
meshAttrToArray (A_M22F  v) = Array ArrFloat  (4 *  V.length v) $ withV V.unsafeWith v
meshAttrToArray (A_M33F  v) = Array ArrFloat  (9 *  V.length v) $ withV V.unsafeWith v
meshAttrToArray (A_M44F  v) = Array ArrFloat  (16 * V.length v) $ withV V.unsafeWith v
meshAttrToArray (A_Int   v) = Array ArrInt32  (1 *  V.length v) $ withV V.unsafeWith v
meshAttrToArray (A_Word  v) = Array ArrWord32 (1 *  V.length v) $ withV V.unsafeWith v

meshAttrToStream :: Buffer -> Int -> MeshAttribute -> Stream Buffer
meshAttrToStream b i (A_Float v) = Stream TFloat b i 0 (V.length v)
meshAttrToStream b i (A_V2F   v) = Stream TV2F b i 0 (V.length v)
meshAttrToStream b i (A_V3F   v) = Stream TV3F b i 0 (V.length v)
meshAttrToStream b i (A_V4F   v) = Stream TV4F b i 0 (V.length v)
meshAttrToStream b i (A_M22F  v) = Stream TM22F b i 0 (V.length v)
meshAttrToStream b i (A_M33F  v) = Stream TM33F b i 0 (V.length v)
meshAttrToStream b i (A_M44F  v) = Stream TM44F b i 0 (V.length v)
meshAttrToStream b i (A_Int   v) = Stream TInt b i 0 (V.length v)
meshAttrToStream b i (A_Word  v) = Stream TWord b i 0 (V.length v)

{-
updateBuffer :: Buffer -> [(Int,Array)] -> IO ()

    | Stream 
        { streamType    :: StreamType
        , streamBuffer  :: b
        , streamArrIdx  :: Int
        , streamStart   :: Int
        , streamLength  :: Int
        }

-- stream of index values (for index buffer)
data IndexStream b
    = IndexStream
    { indexBuffer   :: b
    , indexArrIdx   :: Int
    , indexStart    :: Int
    , indexLength   :: Int
    }
-}
updateMesh :: Mesh -> [(ByteString,MeshAttribute)] -> Maybe MeshPrimitive -> IO ()
updateMesh (Mesh dMA dMP (Just (GPUData _ dS dI))) al mp = do
  -- check type match
  let arrayChk (Array t1 s1 _) (Array t2 s2 _) = t1 == t2 && s1 == s2
      ok = and [T.member n dMA && arrayChk (meshAttrToArray a1) (meshAttrToArray a2) | (n,a1) <- al, let Just a2 = T.lookup n dMA]
  if not ok then putStrLn "updateMesh: attribute mismatch!"
    else do
      forM_ al $ \(n,a) -> do
        case T.lookup n dS of
          Just (Stream _ b i _ _) -> updateBuffer b [(i,meshAttrToArray a)]
          _ -> return ()
{-
      case mp of
        Nothing -> return ()
        Just p -> do
          let ok2 = case (dMP,p) of
                (Just (P_TriangleStripI v1, P_TriangleStripI v2) -> V.length v1 == V.length v2
                (P_TrianglesI v1, P_TrianglesI v2) -> V.length v1 == V.length v2
                (a,b) -> a == b
-}

compileMesh :: Mesh -> IO Mesh
compileMesh (Mesh attrs mPrim Nothing) = do
    let mkIndexBuf v = do
            iBuf <- compileBuffer [Array ArrWord32 (V.length v) $ withV V.unsafeWith v]
            return $! Just $! IndexStream iBuf 0 0 (V.length v)
    vBuf <- compileBuffer [meshAttrToArray a | a <- T.elems attrs]
    (indices,prim) <- case mPrim of
        P_Points            -> return (Nothing,PointList)
        P_TriangleStrip     -> return (Nothing,TriangleStrip)
        P_Triangles         -> return (Nothing,TriangleList)
        P_TriangleStripI v  -> (,TriangleStrip) <$> mkIndexBuf v
        P_TrianglesI v      -> (,TriangleList) <$> mkIndexBuf v
    let streams = T.fromList $! zipWith (\i (n,a) -> (n,meshAttrToStream vBuf i a)) [0..] (T.toList attrs)
        gpuData = GPUData prim streams indices
    return $! Mesh attrs mPrim (Just gpuData)

compileMesh mesh = return mesh

sblToV :: Storable a => [SB.ByteString] -> V.Vector a
sblToV ls = v
  where
    offs o (s:xs) = (o,s):offs (o + SB.length s) xs
    offs _ [] = []
    cnt = sum (map SB.length ls) `div` (sizeOf $ V.head v)
    v = unsafePerformIO $ do
        mv <- MV.new cnt
        MV.unsafeWith mv $ \dst -> forM_ (offs 0 ls) $ \(o,s) ->
            SB.useAsCStringLen s $ \(src,len) -> moveBytes (plusPtr dst o) src len
        V.unsafeFreeze mv

vToSB :: Storable a => V.Vector a -> SB.ByteString
vToSB v = unsafePerformIO $ do
    let len = V.length v * sizeOf (V.head v)
    V.unsafeWith v $ \p -> SB.packCStringLen (castPtr p,len)

instance Storable a => Binary (V.Vector a) where
    put v = put $ vToSB v
    get = do s <- get ; return $ sblToV [s]

instance Binary MeshAttribute where
    put (A_Float a) = putWord8 0 >> put a
    put (A_V2F a)   = putWord8 1 >> put a
    put (A_V3F a)   = putWord8 2 >> put a
    put (A_V4F a)   = putWord8 3 >> put a
    put (A_M22F a)  = putWord8 4 >> put a
    put (A_M33F a)  = putWord8 5 >> put a
    put (A_M44F a)  = putWord8 6 >> put a
    put (A_Int a)   = putWord8 7 >> put a
    put (A_Word a)  = putWord8 8 >> put a
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> A_Float <$> get
            1 -> A_V2F   <$> get
            2 -> A_V3F   <$> get
            3 -> A_V4F   <$> get
            4 -> A_M22F  <$> get
            5 -> A_M33F  <$> get
            6 -> A_M44F  <$> get
            7 -> A_Int   <$> get
            8 -> A_Word  <$> get
            _ -> fail "no parse"

instance Binary MeshPrimitive where
    put P_Points             = putWord8 0
    put P_TriangleStrip      = putWord8 1
    put P_Triangles          = putWord8 2
    put (P_TriangleStripI a) = putWord8 3 >> put a
    put (P_TrianglesI a)     = putWord8 4 >> put a
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> return P_Points
            1 -> return P_TriangleStrip
            2 -> return P_Triangles
            3 -> P_TriangleStripI <$> get
            4 -> P_TrianglesI <$> get
            _ -> fail "no parse"

instance Binary Mesh where
    put (Mesh a b _) = put (T.toList a) >> put b
    get = do
        a <- get
        b <- get
        return $! Mesh (T.fromList a) b Nothing
