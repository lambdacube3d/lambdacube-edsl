module LambdaCube.GL (
    -- language
    module LambdaCube.Language.Type,
    module LambdaCube.Language.ReifyType,
    module LambdaCube.Language.HOAS,
    module LambdaCube.Language,
    Int32,
    Word32,
    uniformBool,
    uniformV2B,
    uniformV3B,
    uniformV4B,

    uniformWord,
    uniformV2U,
    uniformV3U,
    uniformV4U,

    uniformInt,
    uniformV2I,
    uniformV3I,
    uniformV4I,

    uniformFloat,
    uniformV2F,
    uniformV3F,
    uniformV4F,

    uniformM22F,
    uniformM23F,
    uniformM24F,
    uniformM32F,
    uniformM33F,
    uniformM34F,
    uniformM42F,
    uniformM43F,
    uniformM44F,

    uniformFTexture2D,

    -- backend
    Buffer,
    compileBuffer,
    updateBuffer,
    bufferSize,
    arraySize,
    arrayType,

    Renderer,
    compileRenderer,
    slotUniform,
    slotStream,
    uniformSetter,
    render,
    dispose,
    setScreenSize,
    samplerOutput,

    Object,
    addObject,
    removeObject,
    objectUniformSetter,
    enableObject,

    -- texture (temporary)
    compileTexture2DRGBAF,
    updateTexture2DRGBAF,

    -- EDSL reuses types from Core
    V2(..), V3(..), V4(..),
    M22F, M23F, M24F, M32F, M33F, M34F, M42F, M43F, M44F,
    V2F, V3F, V4F, V2I, V3I, V4I, V2U, V3U, V4U, V2B, V3B, V4B,
    --InputType(..),
    PointSpriteCoordOrigin(..),
    PointSize(..),
    PolygonOffset(..),
    FrontFace(..),
    PolygonMode(..),
    ProvokingVertex(..),
    CullMode(..),
    DepthFunction,
    ComparisonFunction(..),
    StencilOperation(..),
    BlendEquation(..),
    BlendingFactor(..),
    LogicOperation(..),
    StencilOps(..),
    StencilTests(..),
    StencilTest(..),
    Filter(..),
    EdgeMode(..),

    -- types for pipeline input
    InputSetter,
    BufferSetter,
    ArrayType(..),
    Array(..),
    Primitive(..),
    StreamType(..),
    Stream(..),
    IndexStream(..),
    TextureData(..),
    SetterFun
) where

import Data.Int
import Data.Word

import LambdaCube.Core.Type

import LambdaCube.Language.ReifyType hiding (Shadow)
import LambdaCube.Language.Type
import LambdaCube.Language.HOAS
import LambdaCube.Language
import qualified LambdaCube.Language.Type as H
import qualified LambdaCube.Language.HOAS as H

import qualified LambdaCube.Core.DeBruijn as U
import LambdaCube.Convert.ToDeBruijn

--import LambdaCube.GL.Backend hiding (compileRenderer)
--import LambdaCube.GL.Compile
import LambdaCube.GL.Data
import LambdaCube.GL.Type
--import LambdaCube.GL.Util (Buffer)
import qualified LambdaCube.GL.Backend as GL

import Control.Monad.State
--import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 as SB
import Data.Trie as T

import LambdaCube.Core.Util.BiMap
import Data.List as L
import qualified Data.IntMap as IM

import System.IO as IO

compileRenderer :: H.GPOutput H.SingleOutput -> IO Renderer
compileRenderer l = do

    let root =  U.toExp dag l'
        (l', dag) = runState (U.unN $ convertGPOutput l) U.emptyDAG
        U.DAG (BiMap _ em) tm _ _ _ = dag
        unis = U.mkExpUni dag
        gunis = U.mkGPUni dag
        dag' = dag {U.expUniverseV = unis, U.gpUniverseV = gunis}
    --print unis
    --print gunis
    print l'
    mapM print $ L.zipWith (\(i,e) (_,t) -> (i,(t,e))) (IM.toList em) (IM.toList tm)

    --(root, dag) <- convert l
    IO.hPutStrLn stderr "GL.compileRenderer"
    GL.compileRenderer dag' root

nullSetter :: ByteString -> String -> a -> IO ()
nullSetter n t _ = return () -- Prelude.putStrLn $ "WARNING: unknown uniform: " ++ SB.unpack n ++ " :: " ++ t

uniformBool  :: ByteString -> Trie InputSetter -> SetterFun Bool
uniformV2B   :: ByteString -> Trie InputSetter -> SetterFun V2B
uniformV3B   :: ByteString -> Trie InputSetter -> SetterFun V3B
uniformV4B   :: ByteString -> Trie InputSetter -> SetterFun V4B

uniformWord  :: ByteString -> Trie InputSetter -> SetterFun Word32
uniformV2U   :: ByteString -> Trie InputSetter -> SetterFun V2U
uniformV3U   :: ByteString -> Trie InputSetter -> SetterFun V3U
uniformV4U   :: ByteString -> Trie InputSetter -> SetterFun V4U

uniformInt   :: ByteString -> Trie InputSetter -> SetterFun Int32
uniformV2I   :: ByteString -> Trie InputSetter -> SetterFun V2I
uniformV3I   :: ByteString -> Trie InputSetter -> SetterFun V3I
uniformV4I   :: ByteString -> Trie InputSetter -> SetterFun V4I

uniformFloat :: ByteString -> Trie InputSetter -> SetterFun Float
uniformV2F   :: ByteString -> Trie InputSetter -> SetterFun V2F
uniformV3F   :: ByteString -> Trie InputSetter -> SetterFun V3F
uniformV4F   :: ByteString -> Trie InputSetter -> SetterFun V4F

uniformM22F   :: ByteString -> Trie InputSetter -> SetterFun M22F
uniformM23F   :: ByteString -> Trie InputSetter -> SetterFun M23F
uniformM24F   :: ByteString -> Trie InputSetter -> SetterFun M24F
uniformM32F   :: ByteString -> Trie InputSetter -> SetterFun M32F
uniformM33F   :: ByteString -> Trie InputSetter -> SetterFun M33F
uniformM34F   :: ByteString -> Trie InputSetter -> SetterFun M34F
uniformM42F   :: ByteString -> Trie InputSetter -> SetterFun M42F
uniformM43F   :: ByteString -> Trie InputSetter -> SetterFun M43F
uniformM44F   :: ByteString -> Trie InputSetter -> SetterFun M44F

uniformFTexture2D   :: ByteString -> Trie InputSetter -> SetterFun TextureData

uniformBool n is = case T.lookup n is of
    Just (SBool (Setter fun)) -> fun
    _   -> nullSetter n "Bool"

uniformV2B n is = case T.lookup n is of
    Just (SV2B (Setter fun)) -> fun
    _   -> nullSetter n "V2B"

uniformV3B n is = case T.lookup n is of
    Just (SV3B (Setter fun)) -> fun
    _   -> nullSetter n "V3B"

uniformV4B n is = case T.lookup n is of
    Just (SV4B (Setter fun)) -> fun
    _   -> nullSetter n "V4B"

uniformWord n is = case T.lookup n is of
    Just (SWord (Setter fun)) -> fun
    _   -> nullSetter n "Word"

uniformV2U n is = case T.lookup n is of
    Just (SV2U (Setter fun)) -> fun
    _   -> nullSetter n "V2U"

uniformV3U n is = case T.lookup n is of
    Just (SV3U (Setter fun)) -> fun
    _   -> nullSetter n "V3U"

uniformV4U n is = case T.lookup n is of
    Just (SV4U (Setter fun)) -> fun
    _   -> nullSetter n "V4U"

uniformInt n is = case T.lookup n is of
    Just (SInt (Setter fun)) -> fun
    _   -> nullSetter n "Int"

uniformV2I n is = case T.lookup n is of
    Just (SV2I (Setter fun)) -> fun
    _   -> nullSetter n "V2I"

uniformV3I n is = case T.lookup n is of
    Just (SV3I (Setter fun)) -> fun
    _   -> nullSetter n "V3I"

uniformV4I n is = case T.lookup n is of
    Just (SV4I (Setter fun)) -> fun
    _   -> nullSetter n "V4I"

uniformFloat n is = case T.lookup n is of
    Just (SFloat (Setter fun)) -> fun
    _   -> nullSetter n "Float"

uniformV2F n is = case T.lookup n is of
    Just (SV2F (Setter fun)) -> fun
    _   -> nullSetter n "V2F"

uniformV3F n is = case T.lookup n is of
    Just (SV3F (Setter fun)) -> fun
    _   -> nullSetter n "V3F"

uniformV4F n is = case T.lookup n is of
    Just (SV4F (Setter fun)) -> fun
    _   -> nullSetter n "V4F"

uniformM22F n is = case T.lookup n is of
    Just (SM22F (Setter fun)) -> fun
    _   -> nullSetter n "M22F"

uniformM23F n is = case T.lookup n is of
    Just (SM23F (Setter fun)) -> fun
    _   -> nullSetter n "M23F"

uniformM24F n is = case T.lookup n is of
    Just (SM24F (Setter fun)) -> fun
    _   -> nullSetter n "M24F"

uniformM32F n is = case T.lookup n is of
    Just (SM32F (Setter fun)) -> fun
    _   -> nullSetter n "M32F"

uniformM33F n is = case T.lookup n is of
    Just (SM33F (Setter fun)) -> fun
    _   -> nullSetter n "M33F"

uniformM34F n is = case T.lookup n is of
    Just (SM34F (Setter fun)) -> fun
    _   -> nullSetter n "M34F"

uniformM42F n is = case T.lookup n is of
    Just (SM42F (Setter fun)) -> fun
    _   -> nullSetter n "M42F"

uniformM43F n is = case T.lookup n is of
    Just (SM43F (Setter fun)) -> fun
    _   -> nullSetter n "M43F"

uniformM44F n is = case T.lookup n is of
    Just (SM44F (Setter fun)) -> fun
    _   -> nullSetter n "M44F"

uniformFTexture2D n is = case T.lookup n is of
    Just (SFTexture2D (Setter fun)) -> fun
    _   -> nullSetter n "FTexture2D"
