{-# LANGUAGE OverloadedStrings #-}
module Render where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Vect.Float
import Data.List
import Foreign
import qualified Data.Trie as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import BSP
import LC_API
import MD3 (MD3Model)
import Q3Patch

{-
    plans:
        - proper render of q3 objects
        - shadow mapping
        - bloom
        - ssao
-}
{-
data Surface
    = Surface
    { srShaderNum      :: Int
    , srFogNum         :: Int
    , srSurfaceType    :: SurfaceType
    , srFirstVertex    :: Int
    , srNumVertices    :: Int
    , srFirstIndex     :: Int
    , srNumIndices     :: Int
    , srLightmapNum    :: Int
    , srLightmapPos    :: Vec2
    , srLightmapSize   :: Vec2
    , srLightmapOrigin :: Vec3
    , srLightmapVec1   :: Vec3
    , srLightmapVec2   :: Vec3
    , srLightmapVec3   :: Vec3
    , srPatchSize      :: (Int,Int)
    }
-}

tesselatePatch :: V.Vector DrawVertex -> Surface -> Int -> (V.Vector DrawVertex,V.Vector Int)
tesselatePatch drawV sf level = (V.concat vl,V.concat il)
  where
    (w,h)   = srPatchSize sf
    gridF :: [DrawVertex] -> [[DrawVertex]]
    gridF l = case splitAt w l of
        (x,[])  -> [x]
        (x,xs)  -> x:gridF xs
    grid        = gridF $ V.toList $ V.take (srNumVertices sf) $ V.drop (srFirstVertex sf) drawV
    controls    = [V.fromList $ concat [take 3 $ drop x l | l <- lines] | x <- [0,2..w-3], y <- [0,2..h-3], let lines = take 3 $ drop y grid]
    patches     = [unsafeTessellate c level | c <- controls]
    (vl,il)     = unzip $ reverse $ snd $ foldl' (\(o,l) (v,i) -> (o+V.length v, (v,V.map (+o) i):l)) (0,[]) patches

addObject' :: Renderer -> ByteString -> Primitive -> Maybe (IndexStream Buffer) -> T.Trie (Stream Buffer) -> [ByteString] -> IO Object
addObject' rndr name prim idx attrs unis = addObject rndr name' prim idx attrs' unis
  where
    attrs'  = T.mapBy (\n a -> if elem n renderAttrs then Just a else Nothing) attrs
    setters = slotStream rndr
    name'  = if T.member name setters then name else "missing shader"
    renderAttrs = T.keys $ case T.lookup name' setters of
        Just (_,x)  -> x
        _           -> error $ "material not found: " ++ show name'

addBSP renderer bsp = do
    let shaderNames = V.map shName $ blShaders bsp
        convertSurface (objs,lenV,arrV,lenI,arrI) sf = case srSurfaceType sf of
            Planar          -> objs'
            TriangleSoup    -> objs'
            -- tesselate, concatenate vertex and index data to fixed vertex and index buffer
            Patch           -> ((lenV, lenV', lenI, lenI', TriangleStrip, name):objs, lenV+lenV', v:arrV, lenI+lenI', i:arrI)
              where
                (v,i) = tesselatePatch drawV sf 5
                lenV' = V.length v
                lenI' = V.length i
            Flare           -> skip
          where
            skip  = ((srFirstVertex sf, srNumVertices sf, srFirstIndex sf, 0, TriangleList, name):objs, lenV, arrV, lenI, arrI)
            objs' = ((srFirstVertex sf, srNumVertices sf, srFirstIndex sf, srNumIndices sf, TriangleList, name):objs, lenV, arrV, lenI, arrI)
            name  = shaderNames V.! (srShaderNum sf)
        drawV = blDrawVertices bsp
        drawI = blDrawIndices bsp
        (objs,_,drawVl,_,drawIl) = V.foldl' convertSurface ([],V.length drawV,[drawV],V.length drawI,[drawI]) $! blSurfaces bsp
        drawV' = V.concat $ reverse drawVl
        drawI' = V.concat $ reverse drawIl

        withV w a f = w a (\p -> f $ castPtr p)
        attribute f = withV SV.unsafeWith $ SV.convert $ V.map f drawV'
        indices     = SV.convert $ V.map fromIntegral drawI' :: SV.Vector Word32
        vertexCount = V.length drawV'

    vertexBuffer <- compileBuffer $
        [ Array ArrFloat (3 * vertexCount) $ attribute dvPosition
        , Array ArrFloat (2 * vertexCount) $ attribute dvDiffuseUV
        , Array ArrFloat (2 * vertexCount) $ attribute dvLightmaptUV
        , Array ArrFloat (3 * vertexCount) $ attribute dvNormal
        , Array ArrFloat (4 * vertexCount) $ attribute dvColor
        ]
    indexBuffer <- compileBuffer [Array ArrWord32 (SV.length indices) $ withV SV.unsafeWith indices]
    let obj (startV,countV,startI,countI,prim,name) = do
            let attrs = T.fromList $
                    [ ("position",      Stream TV3F vertexBuffer 0 startV countV)
                    , ("diffuseUV",     Stream TV2F vertexBuffer 1 startV countV)
                    , ("lightmapUV",    Stream TV2F vertexBuffer 2 startV countV)
                    , ("normal",        Stream TV3F vertexBuffer 3 startV countV)
                    , ("color",         Stream TV4F vertexBuffer 4 startV countV)
                    ]
                index = IndexStream indexBuffer 0 startI countI
            addObject' renderer name prim (Just index) attrs []
    V.mapM obj $ V.fromList objs
    -- foldl' ([],sizeV,[],sizeI,[])    -- list of (startV countV startI countI)
                                        -- size of generated blDrawVertices
                                        -- list of generated blDrawVertices chunks
                                        -- size of generated blDrawIndices
                                        -- list of generated blDrawIndices chunks

addMD3 :: Renderer -> MD3Model -> [ByteString] -> IO Object
addMD3 = undefined
--addMD3 renderer md3 ["world"]

{-
isClusterVisible :: BSPLevel -> Int -> Int -> Bool
isClusterVisible bl a b
    | a >= 0 = 0 /= (visSet .&. (shiftL 1 (b .&. 7)))
    | otherwise = True
  where
    Visibility nvecs szvecs vecs = blVisibility bl
    i = a * szvecs + (shiftR b 3)
    visSet = vecs V.! i

findLeafIdx bl camPos i
    | i >= 0 = if dist >= 0 then findLeafIdx bl camPos f else findLeafIdx bl camPos b
    | otherwise = (-i) - 1
  where 
    node    = blNodes bl V.! i
    (f,b)   = ndChildren node 
    plane   = blPlanes bl V.! ndPlaneNum node
    dist    = plNormal plane `Vect.dotprod` camPos - plDist plane

cullSurfaces :: BSPLevel -> Vect.Vec3 -> Frustum -> V.Vector a -> V.Vector a
cullSurfaces bsp cam frust surfaces = case leafIdx < 0 || leafIdx >= V.length leaves of
    True    -> unsafePerformIO $ print ("findLeafIdx error") >> return surfaces
    False   -> unsafePerformIO $ print ("findLeafIdx ok",leafIdx,camCluster) >> return (V.ifilter (\i _ -> surfaceMask V.! i) surfaces)
  where
    leafIdx = findLeafIdx bsp cam 0
    leaves = blLeaves bsp
    camCluster = lfCluster $ leaves V.! leafIdx
    visibleLeafs = V.filter (\a -> (isClusterVisible bsp camCluster $ lfCluster a) && inFrustum a) leaves
    surfaceMask = unsafePerformIO $ do
        let leafSurfaces = blLeafSurfaces bsp
        mask <- MV.replicate (V.length surfaces) False
        V.forM_ visibleLeafs $ \l ->
            V.forM_ (V.slice (lfFirstLeafSurface l) (lfNumLeafSurfaces l) leafSurfaces) $ \i ->
                MV.write mask i True
        V.unsafeFreeze mask
    inFrustum a = boxInFrustum (lfMaxs a) (lfMins a) frust
-}
