module ThriftUtils (remoteMesh,sblToV,vToSB) where

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

import Thrift.Protocol.Binary
import Thrift.Transport.Handle

import Thrift.ContentProvider_Client
import Thrift.Content_Types

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Trie as T

import qualified LC_Mesh as LC
import LC_API

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

remoteMesh :: ByteString -> IO LC.Mesh
remoteMesh name = do
    let toVInt :: V.Vector Int32 -> V.Vector Int
        toVInt = V.map fromIntegral
    p <- BinaryProtocol <$> hOpen ("localhost", PortNumber 9090)
    Mesh (Just attrs) (Just prim) idx <- downloadMesh (p,p) $ SB.unpack name
    return $ LC.Mesh
        { LC.mAttributes   = T.fromList $ map unpackAttribute attrs
        , LC.mPrimitive    = case (prim,idx) of
            (PT_Points,Nothing)        -> LC.P_Points
            (PT_TriangleStrip,Nothing) -> LC.P_TriangleStrip
            (PT_Triangles,Nothing)     -> LC.P_Triangles
            (PT_TriangleStrip,Just i)  -> LC.P_TriangleStripI $ toV i
            (PT_Triangles,Just i)      -> LC.P_TrianglesI $ toV i
            _                          -> error "Invalid primitive!"

        , LC.mGPUData      = Nothing
        }
