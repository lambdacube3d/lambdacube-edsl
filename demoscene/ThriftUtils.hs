module ThriftUtils where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Int
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Network
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector as Vector

import Thrift.Protocol.Binary
import Thrift.Transport.Handle

import Thrift.ContentProvider_Client
import Thrift.Content_Types

import System.IO.Unsafe (unsafePerformIO)
import GHC.IO.Handle (Handle)

import qualified Data.Trie as T

import qualified LambdaCube.GL.Mesh as LC
import qualified FCurve as FC
import LambdaCube.GL

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

toV :: Storable a => [LB.ByteString] -> V.Vector a
toV lb = sblToV $ concatMap LB.toChunks lb

unpackAttribute :: VertexAttribute -> (ByteString,LC.MeshAttribute)
unpackAttribute (VertexAttribute (Just an) (Just at) (Just ad)) = (,) (SB.pack an) $ case at of
    AT_Float -> LC.A_Float $ toV ad
    AT_Vec2  -> LC.A_V2F   $ toV ad
    AT_Vec3  -> LC.A_V3F   $ toV ad
    AT_Vec4  -> LC.A_V4F   $ toV ad
    AT_Mat2  -> LC.A_M22F  $ toV ad
    AT_Mat3  -> LC.A_M33F  $ toV ad
    AT_Mat4  -> LC.A_M44F  $ toV ad
    AT_Int   -> LC.A_Int   $ toV ad
    AT_Word  -> LC.A_Word  $ toV ad 


type Slot = (BinaryProtocol Handle,BinaryProtocol Handle)
protocol' :: String -> IO Slot
protocol' n = do
    p <- BinaryProtocol <$> hOpen (n, PortNumber 50001)
    return (p,p)

protocol :: IO Slot
protocol = protocol' "localhost"

remoteMesh :: Slot -> ByteString -> IO LC.Mesh
remoteMesh slot name = do
    Just mesh <- remoteMesh' slot name
    return mesh

remoteMesh' :: Slot -> ByteString -> IO (Maybe LC.Mesh)
remoteMesh' slot name = do
    let toVInt :: V.Vector Int32 -> V.Vector Int
        toVInt = V.map fromIntegral
    --proto <- protocol
    mesh <- downloadMesh slot $ SB.unpack name
    return $ case mesh of
        Mesh (Just attrs) (Just prim) idx -> Just $ LC.Mesh
            { LC.mAttributes    = T.fromList $ map unpackAttribute attrs
            , LC.mPrimitive     = case (prim,idx) of
                (PT_Points,Nothing)        -> LC.P_Points
                (PT_TriangleStrip,Nothing) -> LC.P_TriangleStrip
                (PT_Triangles,Nothing)     -> LC.P_Triangles
                (PT_TriangleStrip,Just i)  -> LC.P_TriangleStripI $ toV i
                (PT_Triangles,Just i)      -> LC.P_TrianglesI $ toV i
                _                          -> error "Invalid primitive!"
            , LC.mGPUData       = Nothing
            }

        _   -> Nothing

remoteFCurve :: Slot -> ByteString -> ByteString -> IO FC.Value
remoteFCurve slot objName dataPath = do
    Just fcurve <- remoteFCurve' slot objName dataPath
    return fcurve

remoteFCurve' :: Slot -> ByteString -> ByteString -> IO (Maybe FC.Value)
remoteFCurve' slot objName dataPath = do
    fcurve <- downloadFCurve slot (SB.unpack objName) (SB.unpack dataPath)
    let cvtI a = case a of
            I_Constant  -> FC.IConstant
            I_Linear    -> FC.ILinear
            I_Bezier    -> FC.IBezier

        cvtE a = case a of
            E_Constant  -> FC.EConstant
            E_Linear    -> FC.ELinear

        d2f :: Double -> Float
        d2f = realToFrac

        cvtS (Segment (Just i) (Just lt) (Just lv) (Just t) (Just v) (Just rt) (Just rv)) =
            FC.Segment (cvtI i) (d2f lt) (d2f lv) (d2f t) (d2f v) (d2f rt) (d2f rv)
        cvtS a = error $ "missing data: " ++ show a

        cvtFC (FCurve (Just e) (Just l)) = FC.FCurve (cvtE e) (Vector.fromList $ map cvtS l)
        cvtFC a = error $ "missing data: " ++ show a
    return $ case fcurve of
        []  -> Nothing
        l   -> Just $ FC.Value (SB.unpack dataPath) (map cvtFC l)
