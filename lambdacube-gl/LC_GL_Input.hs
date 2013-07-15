module LC_GL_Input where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.IntMap (IntMap)
import Data.Trie (Trie)
import Data.Vector (Vector,(//),(!))
import qualified Data.IntMap as IM
import qualified Data.Trie as T
import qualified Data.Vector as V
import Data.Word

import LC_G_APIType
import LC_U_APIType
import LC_GL_Type
import LC_GL_Util

import qualified LC_B2_IR as IR

schemaFromPipeline :: IR.Pipeline -> PipelineSchema
schemaFromPipeline a = PipelineSchema (T.fromList sl) (foldl T.unionL T.empty ul)
  where
    (sl,ul) = unzip [((n,SlotSchema p (fmap cvt s)),u) | IR.Slot n u s p <- V.toList $ IR.slots a]
    cvt a = case toStreamType a of
        Just v  -> v
        Nothing -> error "internal error (schemaFromPipeline)"

mkUniform :: [(ByteString,InputType)] -> IO (Trie InputSetter, Trie GLUniform)
mkUniform l = do
    unisAndSetters <- forM l $ \(n,t) -> do
        (setSeparated, setter, setBuffer) <- mkUniformSetter t
        return ((n,GLUniform setSeparated setBuffer),(n,setter))
    let (unis,setters) = unzip unisAndSetters
    return (T.fromList setters, T.fromList unis)

mkGLPipelineInput :: PipelineSchema -> IO GLPipelineInput
mkGLPipelineInput sch = do
    let sm  = T.fromList $ zip (T.keys $ slots sch) [0..]
        len = T.size sm
    (setters,unis) <- mkUniform $ T.toList $ uniforms sch
    seed <- newIORef 0
    objs <- newIORef $ V.replicate len $ IM.empty
    size <- newIORef (0,0)
    return $ GLPipelineInput
        { schema        = sch
        , slotMap       = sm
        , objects       = objs
        , objSeed       = seed
        , uniformSetter = setters
        , uniformSetup  = unis
        , screenSize    = size
        }

-- object
addObject :: GLPipelineInput -> ByteString -> Primitive -> Maybe (IndexStream Buffer) -> Trie (Stream Buffer) -> [ByteString] -> IO Object
addObject p slotName prim indices attribs uniformNames = do
    let sch = schema p
    forM_ uniformNames $ \n -> case T.lookup n (uniforms sch) of
        Nothing -> throw $ userError $ "Unknown uniform: " ++ show n
        _ -> return ()
    case T.lookup slotName (slots sch) of
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
                
    let slotIdx = case slotName `T.lookup` slotMap p of
            Nothing -> error "internal error (slot index)"
            Just i  -> i
        seed = objSeed p
    order <- newIORef 0
    enabled <- newIORef True
    index <- readIORef seed
    modifyIORef seed (1+)
    (setters,unis) <- mkUniform [(n,t) | n <- uniformNames, let Just t = T.lookup n (uniforms sch)]
    let obj = Object
            { objSlot       = slotName
            , objPrimitive  = prim
            , objIndices    = indices
            , objAttributes = attribs
            , objUniSetter  = setters
            , objUniSetup   = unis
            , objOrder      = order
            , objEnabled    = enabled
            , objId         = index
            }
    modifyIORef (objects p) $ \v -> v // [(slotIdx,IM.insert index obj (v ! slotIdx))]
    return obj

removeObject :: GLPipelineInput -> Object -> IO ()
removeObject p obj = do
    let slotIdx = case (objSlot obj) `T.lookup` slotMap p of
            Nothing -> error "internal error (slot index)"
            Just i  -> i
    modifyIORef (objects p) $ \v -> v // [(slotIdx,IM.delete (objId obj) (v ! slotIdx))]

enableObject :: Object -> Bool -> IO ()
enableObject obj b = writeIORef (objEnabled obj) b

setObjectOrder :: Object -> Int -> IO ()
setObjectOrder obj i = writeIORef (objOrder obj) i

objectUniformSetter :: Object -> Trie InputSetter
objectUniformSetter = objUniSetter

setScreenSize :: GLPipelineInput -> Word -> Word -> IO ()
setScreenSize p w h = writeIORef (screenSize p) (w,h)
