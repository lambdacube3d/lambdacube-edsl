module Backend.GL.Input where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 (ByteString,pack)
import Data.IORef
import Data.IntMap (IntMap)
import Data.Trie (Trie)
import Data.Trie.Convenience as T
import Data.Vector (Vector,(//),(!))
import Data.Word
import Foreign
import qualified Data.ByteString.Char8 as SB
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Map as Map
import qualified Data.Trie as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I

import Graphics.Rendering.OpenGL.Raw.Core33

import IR as IR
import Backend.GL.Type as T
import Backend.GL.Util

import qualified IR as IR

schemaFromPipeline :: IR.Pipeline -> PipelineSchema
schemaFromPipeline a = PipelineSchema (T.fromList sl) (foldl T.unionL T.empty ul)
  where
    (sl,ul) = unzip [((pack n,SlotSchema p (fmap cvt (toTrie s))),toTrie u) | IR.Slot n u s p _ <- V.toList $ IR.slots a]
    cvt a = case toStreamType a of
        Just v  -> v
        Nothing -> error "internal error (schemaFromPipeline)"

mkUniform :: [(ByteString,InputType)] -> IO (Trie InputSetter, Trie GLUniform)
mkUniform l = do
    unisAndSetters <- forM l $ \(n,t) -> do
        (uni, setter) <- mkUniformSetter t
        return ((n,uni),(n,setter))
    let (unis,setters) = unzip unisAndSetters
    return (T.fromList setters, T.fromList unis)

mkGLPipelineInput :: PipelineSchema -> IO GLPipelineInput
mkGLPipelineInput sch = do
    let sm  = T.fromList $ zip (T.keys $ T.slots sch) [0..]
        len = T.size sm
    (setters,unis) <- mkUniform $ T.toList $ uniforms sch
    seed <- newIORef 0
    slotV <- V.replicateM len $ newIORef (GLSlot IM.empty V.empty Ordered)
    size <- newIORef (0,0)
    ppls <- newIORef $ V.singleton Nothing
    return $ GLPipelineInput
        { schema        = sch
        , slotMap       = sm
        , slotVector    = slotV
        , objSeed       = seed
        , uniformSetter = setters
        , uniformSetup  = unis
        , screenSize    = size
        , pipelines     = ppls
        }

-- object
addObject :: GLPipelineInput -> ByteString -> Primitive -> Maybe (IndexStream Buffer) -> Trie (Stream Buffer) -> [ByteString] -> IO Object
addObject input slotName prim indices attribs uniformNames = do
    let sch = schema input
    forM_ uniformNames $ \n -> case T.lookup n (uniforms sch) of
        Nothing -> throw $ userError $ "Unknown uniform: " ++ show n
        _ -> return ()
    case T.lookup slotName (T.slots sch) of
        Nothing -> throw $ userError $ "Unknown slot: " ++ show slotName
        Just (SlotSchema sPrim sAttrs) -> do
            when (sPrim /= (primitiveToFetchPrimitive prim)) $ throw $ userError $
                "Primitive mismatch for slot (" ++ show slotName ++ ") expected " ++ show sPrim  ++ " but got " ++ show prim
            let sType = fmap streamToStreamType attribs
            when (sType /= sAttrs) $ throw $ userError $ unlines $ 
                [ "Attribute stream mismatch for slot (" ++ show slotName ++ ") expected "
                , show sAttrs
                , " but got "
                , show sType
                ]
                
    let slotIdx = case slotName `T.lookup` slotMap input of
            Nothing -> error "internal error (slot index)"
            Just i  -> i
        seed = objSeed input
    order <- newIORef 0
    enabled <- newIORef True
    index <- readIORef seed
    modifyIORef seed (1+)
    (setters,unis) <- mkUniform [(n,t) | n <- uniformNames, let Just t = T.lookup n (uniforms sch)]
    cmdsRef <- newIORef (V.singleton V.empty)
    let obj = Object
            { objSlot       = slotIdx
            , objPrimitive  = prim
            , objIndices    = indices
            , objAttributes = attribs
            , objUniSetter  = setters
            , objUniSetup   = unis
            , objOrder      = order
            , objEnabled    = enabled
            , objId         = index
            , objCommands   = cmdsRef
            }

    modifyIORef (slotVector input ! slotIdx) $ \(GLSlot objs _ _) -> GLSlot (IM.insert index obj objs) V.empty Generate

    -- generate GLObjectCommands for the new object
    {-
        foreach pipeline:
            foreach realted program:
                generate commands
    -}
    ppls <- readIORef $ pipelines input
    let topUnis = uniformSetup input
    cmds <- V.forM ppls $ \mp -> case mp of
        Nothing -> print "nothing to do??" >> return V.empty
        Just p  -> do
            Just ic <- readIORef $ glInput p
            case icSlotMapInputToPipeline ic ! slotIdx of
                Nothing         -> do
                    putStrLn $ " ** slot is not used!"
                    return V.empty   -- this slot is not used in that pipeline
                Just pSlotIdx   -> do
                    putStrLn "slot is used!" 
                    --where
                    let emptyV = V.replicate (V.length $ glPrograms p) []
                    return $ emptyV // [(prgIdx,createObjectCommands (glTexUnitMapping p) topUnis obj (glPrograms p ! prgIdx))| prgIdx <- glSlotPrograms p ! pSlotIdx]
    writeIORef cmdsRef cmds
    return obj

removeObject :: GLPipelineInput -> Object -> IO ()
removeObject p obj = modifyIORef (slotVector p ! objSlot obj) $ \(GLSlot objs _ _) -> GLSlot (IM.delete (objId obj) objs) V.empty Generate

enableObject :: Object -> Bool -> IO ()
enableObject obj b = writeIORef (objEnabled obj) b

setObjectOrder :: GLPipelineInput -> Object -> Int -> IO ()
setObjectOrder p obj i = do
    writeIORef (objOrder obj) i
    modifyIORef (slotVector p ! objSlot obj) $ \(GLSlot objs sorted _) -> GLSlot objs sorted Reorder

objectUniformSetter :: Object -> Trie InputSetter
objectUniformSetter = objUniSetter

setScreenSize :: GLPipelineInput -> Word -> Word -> IO ()
setScreenSize p w h = writeIORef (screenSize p) (w,h)

sortSlotObjects :: GLPipelineInput -> IO ()
sortSlotObjects p = V.forM_ (slotVector p) $ \slotRef -> do
    GLSlot objMap sortedV ord <- readIORef slotRef
    let cmpFun (a,_) (b,_) = a `compare` b
        doSort objs = do
            ordObjsM <- V.thaw objs
            I.sortBy cmpFun ordObjsM
            ordObjs <- V.freeze ordObjsM
            writeIORef slotRef (GLSlot objMap ordObjs Ordered)
    case ord of
        Ordered -> return ()
        Generate -> do
            objs <- V.forM (V.fromList $ IM.elems objMap) $ \obj -> do
                ord <- readIORef $ objOrder obj
                return (ord,obj)
            doSort objs
        Reorder -> do
            objs <- V.forM sortedV $ \(_,obj) -> do
                ord <- readIORef $ objOrder obj
                return (ord,obj)
            doSort objs

createObjectCommands :: Trie (IORef GLint) -> Trie GLUniform -> Object -> GLProgram -> [GLObjectCommand]
createObjectCommands texUnitMap topUnis obj prg = objUniCmds ++ objStreamCmds ++ [objDrawCmd]
  where
    -- object draw command
    objDrawCmd = case objIndices obj of
        Nothing -> GLDrawArrays prim 0 (fromIntegral count)
        Just (IndexStream (Buffer arrs bo) arrIdx start idxCount) -> GLDrawElements prim (fromIntegral idxCount) idxType bo ptr
          where
            ArrayDesc arrType arrLen arrOffs arrSize = arrs ! arrIdx
            idxType = arrayTypeToGLType arrType
            ptr    = intPtrToPtr $! fromIntegral (arrOffs + start * sizeOfArrayType arrType)
      where
        objAttrs = objAttributes obj
        prim = primitiveToGLType $ objPrimitive obj
        count = head [c | Stream _ _ _ _ c <- T.elems objAttrs]

    -- object uniform commands
    -- texture slot setup commands
    objUniCmds = uniCmds ++ texCmds
      where
        uniCmds = [GLSetUniform i u | (n,i) <- uniMap, let u = T.lookupWithDefault (topUni n) n objUnis]
        uniMap  = T.toList $ inputUniforms prg
        topUni n = T.lookupWithDefault (error "internal error (createObjectCommands)!") n topUnis
        objUnis = objUniSetup obj
        texUnis = S.toList $ inputTextureUniforms prg
        texCmds = [ GLBindTexture (inputTypeToTextureTarget $ uniInputType u) texUnit u
                  | n <- texUnis
                  , let u = T.lookupWithDefault (topUni n) n objUnis
                  , let texUnit = T.lookupWithDefault (error "internal error (createObjectCommands - Texture Unit)") n texUnitMap
                  ]
        uniInputType (GLUniform ty _) = ty

    -- object attribute stream commands
    objStreamCmds = [attrCmd i s | (i,name) <- T.elems attrMap, let Just s = T.lookup name objAttrs]
      where 
        attrMap = inputStreams prg
        objAttrs = objAttributes obj
        attrCmd i s = case s of
            Stream ty (Buffer arrs bo) arrIdx start len -> case ty of
                TWord   -> setIntAttrib 1
                TV2U    -> setIntAttrib 2
                TV3U    -> setIntAttrib 3
                TV4U    -> setIntAttrib 4
                TInt    -> setIntAttrib 1
                TV2I    -> setIntAttrib 2
                TV3I    -> setIntAttrib 3
                TV4I    -> setIntAttrib 4
                TFloat  -> setFloatAttrib 1
                TV2F    -> setFloatAttrib 2
                TV3F    -> setFloatAttrib 3
                TV4F    -> setFloatAttrib 4
                TM22F   -> setFloatAttrib 4
                TM23F   -> setFloatAttrib 6
                TM24F   -> setFloatAttrib 8
                TM32F   -> setFloatAttrib 6
                TM33F   -> setFloatAttrib 9
                TM34F   -> setFloatAttrib 12
                TM42F   -> setFloatAttrib 8
                TM43F   -> setFloatAttrib 12
                TM44F   -> setFloatAttrib 16
              where
                setFloatAttrib n = GLSetVertexAttribArray i bo n glType (ptr n)
                setIntAttrib n = GLSetVertexAttribIArray i bo n glType (ptr n)
                ArrayDesc arrType arrLen arrOffs arrSize = arrs ! arrIdx
                glType = arrayTypeToGLType arrType
                ptr compCnt   = intPtrToPtr $! fromIntegral (arrOffs + start * fromIntegral compCnt * sizeOfArrayType arrType)

            -- constant generic attribute
            constAttr -> GLSetVertexAttrib i constAttr

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
    Just (SBool fun)    -> fun
    _   -> nullSetter n "Bool"

uniformV2B n is = case T.lookup n is of
    Just (SV2B fun)    -> fun
    _   -> nullSetter n "V2B"

uniformV3B n is = case T.lookup n is of
    Just (SV3B fun)    -> fun
    _   -> nullSetter n "V3B"

uniformV4B n is = case T.lookup n is of
    Just (SV4B fun)    -> fun
    _   -> nullSetter n "V4B"

uniformWord n is = case T.lookup n is of
    Just (SWord fun)    -> fun
    _   -> nullSetter n "Word"

uniformV2U n is = case T.lookup n is of
    Just (SV2U fun)    -> fun
    _   -> nullSetter n "V2U"

uniformV3U n is = case T.lookup n is of
    Just (SV3U fun)    -> fun
    _   -> nullSetter n "V3U"

uniformV4U n is = case T.lookup n is of
    Just (SV4U fun)    -> fun
    _   -> nullSetter n "V4U"

uniformInt n is = case T.lookup n is of
    Just (SInt fun)    -> fun
    _   -> nullSetter n "Int"

uniformV2I n is = case T.lookup n is of
    Just (SV2I fun)    -> fun
    _   -> nullSetter n "V2I"

uniformV3I n is = case T.lookup n is of
    Just (SV3I fun)    -> fun
    _   -> nullSetter n "V3I"

uniformV4I n is = case T.lookup n is of
    Just (SV4I fun)    -> fun
    _   -> nullSetter n "V4I"

uniformFloat n is = case T.lookup n is of
    Just (SFloat fun)    -> fun
    _   -> nullSetter n "Float"

uniformV2F n is = case T.lookup n is of
    Just (SV2F fun)    -> fun
    _   -> nullSetter n "V2F"

uniformV3F n is = case T.lookup n is of
    Just (SV3F fun)    -> fun
    _   -> nullSetter n "V3F"

uniformV4F n is = case T.lookup n is of
    Just (SV4F fun)    -> fun
    _   -> nullSetter n "V4F"

uniformM22F n is = case T.lookup n is of
    Just (SM22F fun)    -> fun
    _   -> nullSetter n "M22F"

uniformM23F n is = case T.lookup n is of
    Just (SM23F fun)    -> fun
    _   -> nullSetter n "M23F"

uniformM24F n is = case T.lookup n is of
    Just (SM24F fun)    -> fun
    _   -> nullSetter n "M24F"

uniformM32F n is = case T.lookup n is of
    Just (SM32F fun)    -> fun
    _   -> nullSetter n "M32F"

uniformM33F n is = case T.lookup n is of
    Just (SM33F fun)    -> fun
    _   -> nullSetter n "M33F"

uniformM34F n is = case T.lookup n is of
    Just (SM34F fun)    -> fun
    _   -> nullSetter n "M34F"

uniformM42F n is = case T.lookup n is of
    Just (SM42F fun)    -> fun
    _   -> nullSetter n "M42F"

uniformM43F n is = case T.lookup n is of
    Just (SM43F fun)    -> fun
    _   -> nullSetter n "M43F"

uniformM44F n is = case T.lookup n is of
    Just (SM44F fun)    -> fun
    _   -> nullSetter n "M44F"

uniformFTexture2D n is = case T.lookup n is of
    Just (SFTexture2D fun)    -> fun
    _   -> nullSetter n "FTexture2D"
