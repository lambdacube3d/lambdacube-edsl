module Input where

import Prelude.Unsafe (unsafeIndex)
import Control.Bind
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception
import qualified Data.Map as Map
import qualified Data.StrMap as StrMap
import qualified Data.StrMap.Unsafe as StrMap
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Tuple
import Data.Array
import Type
import IR
import Util

-- API
schemaFromPipeline :: IR.Pipeline -> GFX PipelineSchema
schemaFromPipeline ppl = do
  sl <- flip traverse ppl.slots $ \s -> do
    a <- traverse toStreamType s.slotStreams
    return $ Tuple s.slotName {primitive: s.slotPrimitive, attributes: a}
  let ul = map (\s -> s.slotUniforms) ppl.slots
  return $
    { slots: StrMap.fromList sl
    , uniforms: foldl StrMap.union (StrMap.empty :: StrMap.StrMap InputType) ul
    }

mkUniform :: [Tuple String InputType] -> GFX (Tuple (StrMap.StrMap InputSetter) (StrMap.StrMap GLUniform))
mkUniform l = do
  unisAndSetters <- flip traverse l $ \(Tuple n t) -> do
    (Tuple uni setter) <- mkUniformSetter t
    return $ Tuple (Tuple n uni) (Tuple n setter)
  let fun (Tuple unis setters) = Tuple (StrMap.fromList setters) (StrMap.fromList unis)
  return $ fun $ unzip unisAndSetters

mkWebGLPipelineInput :: PipelineSchema -> GFX WebGLPipelineInput
mkWebGLPipelineInput sch = do
  let sm = StrMap.fromList $ zip (StrMap.keys sch.slots) (0..len)
      len = StrMap.size sch.slots
  Tuple setters unis <- mkUniform $ StrMap.toList $ sch.uniforms
  slotV <- replicateM len $ newRef {objectMap: Map.empty :: Map.Map Int GLObject, sortedObjects: [], orderJob: Ordered}
  seed <- newRef 0
  size <- newRef (V2 0 0)
  ppls <- newRef [Nothing]
  return $
    { schema        : sch
    , slotMap       : sm
    , slotVector    : slotV
    , objSeed       : seed
    , uniformSetter : setters
    , uniformSetup  : unis
    , screenSize    : size
    , pipelines     : ppls
    }

addObject :: WebGLPipelineInput -> String -> Primitive -> Maybe (IndexStream Buffer) -> StrMap.StrMap (Stream Buffer) -> [String] -> GFX GLObject
addObject input slotName prim indices attribs uniformNames = do
    flip traverse uniformNames $ \n -> case StrMap.lookup n input.schema.uniforms of
        Nothing -> throwException $ error $ "Unknown uniform: " ++ show n
        _ -> return unit
    case StrMap.lookup slotName input.schema.slots of
        Nothing -> throwException $ error $ "Unknown slot: " ++ slotName
        Just slotSchema -> do
            when (slotSchema.primitive /= (primitiveToFetchPrimitive prim)) $ throwException $ error $
                "Primitive mismatch for slot (" ++ show slotName ++ ") expected " ++ show slotSchema.primitive  ++ " but got " ++ show prim
            let sType = streamToStreamType <$> attribs
            when (sType /= slotSchema.attributes) $ throwException $ error $ unlines $ 
                [ "Attribute stream mismatch for slot (" ++ show slotName ++ ") expected "
                , show slotSchema.attributes
                , " but got "
                , show sType
                ]

    slotIdx <- case slotName `StrMap.lookup` input.slotMap of
      Nothing -> throwException $ error "internal error (slot index)"
      Just i  -> return i
    order <- newRef 0
    enabled <- newRef true
    index <- readRef input.objSeed
    modifyRef input.objSeed (1+)
    Tuple setters unis <- mkUniform =<< (flip traverse uniformNames $ \n -> case StrMap.lookup n input.schema.uniforms of
      Nothing -> throwException $ error "internal error (uniform setter not found)"
      Just t -> return $ Tuple n t)
    cmdsRef <- newRef [[]]
    let obj =
          { slot       : slotIdx
          , primitive  : prim
          , indices    : indices
          , attributes : attribs
          , uniSetter  : setters
          , uniSetup   : unis
          , order      : order
          , enabled    : enabled
          , id         : index
          , commands   : cmdsRef
          }

    modifyRef (input.slotVector `unsafeIndex` slotIdx) $ \s -> {objectMap: Map.insert index obj s.objectMap, sortedObjects: [], orderJob: Generate}

    -- generate GLObjectCommands for the new object
    {-
        foreach pipeline:
            foreach realted program:
                generate commands
    -}
    ppls <- readRef input.pipelines
    cmds <- flip traverse ppls $ \mp -> case mp of
        Nothing -> return []
        Just p  -> do
            Just (InputConnection ic) <- readRef p.input
            case ic.slotMapInputToPipeline `unsafeIndex` slotIdx of
                Nothing -> do
                    return []   -- this slot is not used in that pipeline
                Just pSlotIdx -> do
                    let emptyV = replicate (length p.programs) []
                        addCmds v prgIdx = updateAt prgIdx (createObjectCommands {-(glTexUnitMapping p)-}{} input.uniformSetup obj (p.programs `unsafeIndex` prgIdx)) v
                    return $ foldl addCmds emptyV $ p.slotPrograms `unsafeIndex` pSlotIdx
    writeRef cmdsRef cmds
    return obj

removeObject :: WebGLPipelineInput -> GLObject -> GFX Unit
removeObject p obj = modifyRef (p.slotVector `unsafeIndex` obj.slot) $ \s -> {objectMap:Map.delete obj.id s.objectMap, sortedObjects:[], orderJob:Generate}

enableObject :: GLObject -> Bool -> GFX Unit
enableObject obj b = writeRef obj.enabled b

setObjectOrder :: WebGLPipelineInput -> GLObject -> Int -> GFX Unit
setObjectOrder p obj i = do
    writeRef obj.order i
    modifyRef (p.slotVector `unsafeIndex` obj.slot) $ \s -> s {orderJob = Reorder}

objectUniformSetter :: GLObject -> StrMap.StrMap InputSetter
objectUniformSetter o = o.uniSetter

setScreenSize :: WebGLPipelineInput -> V2U -> GFX Unit
setScreenSize p s = writeRef p.screenSize s

sortSlotObjects :: WebGLPipelineInput -> GFX Unit
sortSlotObjects p = do
  flip traverse p.slotVector $ \slotRef -> do
    slot <- readRef slotRef
    let cmpFun (Tuple a _) (Tuple b _) = a `compare` b
        doSort objs = writeRef slotRef $ slot {sortedObjects = sortBy cmpFun objs, orderJob = Ordered}
    case slot.orderJob of
        Ordered -> return unit
        Generate -> do
            objs <- flip traverse (Map.values slot.objectMap) $ \obj -> do
                ord <- readRef obj.order
                return $ Tuple ord obj
            doSort objs
        Reorder -> do
            objs <- flip traverse slot.sortedObjects $ \(Tuple _ obj) -> do
                ord <- readRef obj.order
                return (Tuple ord obj)
            doSort objs
  return unit

nullSetter :: forall a . String -> String -> a -> GFX Unit
nullSetter n t _ = return unit -- Prelude.putStrLn $ "WARNING: unknown uniform: " ++ n ++ " :: " ++ t

uniformBool :: String -> StrMap.StrMap InputSetter -> SetterFun Bool
uniformBool n is =
  case StrMap.lookup n is of
  Just (SBool fun) -> fun
  _ -> nullSetter n "Bool"

uniformV2B :: String -> StrMap.StrMap InputSetter -> SetterFun V2B
uniformV2B n is = case StrMap.lookup n is of
  Just (SV2B fun) -> fun
  _ -> nullSetter n "V2B"

uniformV3B :: String -> StrMap.StrMap InputSetter -> SetterFun V3B
uniformV3B n is = case StrMap.lookup n is of
  Just (SV3B fun) -> fun
  _ -> nullSetter n "V3B"

uniformV4B :: String -> StrMap.StrMap InputSetter -> SetterFun V4B
uniformV4B n is =case StrMap.lookup n is of
  Just (SV4B fun) -> fun
  _ -> nullSetter n "V4B"

uniformInt :: String -> StrMap.StrMap InputSetter -> SetterFun Int32
uniformInt n is = case StrMap.lookup n is of
  Just (SInt fun) -> fun
  _ -> nullSetter n "Int"

uniformV2I :: String -> StrMap.StrMap InputSetter -> SetterFun V2I
uniformV2I n is = case StrMap.lookup n is of
  Just (SV2I fun) -> fun
  _ -> nullSetter n "V2I"

uniformV3I :: String -> StrMap.StrMap InputSetter -> SetterFun V3I
uniformV3I n is = case StrMap.lookup n is of
  Just (SV3I fun) -> fun
  _ -> nullSetter n "V3I"

uniformV4I :: String -> StrMap.StrMap InputSetter -> SetterFun V4I
uniformV4I n is = case StrMap.lookup n is of
  Just (SV4I fun) -> fun
  _ -> nullSetter n "V4I"

uniformFloat :: String -> StrMap.StrMap InputSetter -> SetterFun Float
uniformFloat n is = case StrMap.lookup n is of
  Just (SFloat fun) -> fun
  _ -> nullSetter n "Float"

uniformV2F :: String -> StrMap.StrMap InputSetter -> SetterFun V2F
uniformV2F n is = case StrMap.lookup n is of
  Just (SV2F fun) -> fun
  _ -> nullSetter n "V2F"

uniformV3F :: String -> StrMap.StrMap InputSetter -> SetterFun V3F
uniformV3F n is = case StrMap.lookup n is of
  Just (SV3F fun) -> fun
  _ -> nullSetter n "V3F"

uniformV4F :: String -> StrMap.StrMap InputSetter -> SetterFun V4F
uniformV4F n is = case StrMap.lookup n is of
  Just (SV4F fun) -> fun
  _ -> nullSetter n "V4F"

uniformM22F :: String -> StrMap.StrMap InputSetter -> SetterFun M22F
uniformM22F n is = case StrMap.lookup n is of
  Just (SM22F fun) -> fun
  _ -> nullSetter n "M22F"

uniformM33F :: String -> StrMap.StrMap InputSetter -> SetterFun M33F
uniformM33F n is = case StrMap.lookup n is of
  Just (SM33F fun) -> fun
  _ -> nullSetter n "M33F"

uniformM44F :: String -> StrMap.StrMap InputSetter -> SetterFun M44F
uniformM44F n is = case StrMap.lookup n is of
  Just (SM44F fun) -> fun
  _ -> nullSetter n "M44F"

createObjectCommands :: {} -> {-Trie (IORef GLint) -> -}StrMap.StrMap GLUniform -> GLObject -> GLProgram -> [GLObjectCommand]
createObjectCommands texUnitMap topUnis obj prg = concat [objUniCmds, objStreamCmds, [objDrawCmd]]
  where
    -- object draw command
    objDrawCmd = let
        prim = primitiveToGLType obj.primitive
        streamLen a = case a of
          Stream s  -> [s.length]
          _         -> []
        count = (concatMap streamLen $ StrMap.values obj.attributes) `unsafeIndex` 0
      in case obj.indices of
        Nothing -> GLDrawArrays prim 0 count
        Just is -> let 
            a = is.buffer.arrays `unsafeIndex` is.arrIdx
            idxType = arrayTypeToGLType a.arrType
            ptr    = a.arrOffset + is.start * sizeOfArrayType a.arrType
          in GLDrawElements prim is.length idxType is.buffer.glBuffer ptr

    -- object uniform commands
    -- texture slot setup commands
    objUniCmds = uniCmds -- `append` texCmds
      where
        uniMap  = StrMap.toList prg.inputUniforms
        uniCmds = flip map uniMap $ \(Tuple n i) -> GLSetUniform i $ case StrMap.lookup n obj.uniSetup of
          Nothing -> topUnis `StrMap.unsafeIndex` n
          Just u  -> u
        {-
        texUnis = S.toList $ inputTextureUniforms prg
        texCmds = [ GLBindTexture (inputTypeToTextureTarget $ uniInputType u) texUnit u
                  | n <- texUnis
                  , let u = T.lookupWithDefault (topUni n) n objUnis
                  , let texUnit = T.lookupWithDefault (error "internal error (createObjectCommands - Texture Unit)") n texUnitMap
                  ]
        uniInputType (GLUniform ty _) = ty
        -}

    -- object attribute stream commands
    objStreamCmds = flip map (StrMap.values prg.inputStreams) $ \is -> let
        s = obj.attributes `StrMap.unsafeIndex` is.slotAttribute
        i = is.location
      in case s of
          Stream s -> let
              desc = s.buffer.arrays `unsafeIndex` s.arrIdx
              glType      = arrayTypeToGLType desc.arrType
              ptr compCnt = desc.arrOffset + s.start * compCnt * sizeOfArrayType desc.arrType
              setFloatAttrib n = GLSetVertexAttribArray i s.buffer.glBuffer n glType (ptr n)
            in setFloatAttrib $ case s.sType of
                TFloat  -> 1
                TV2F    -> 2
                TV3F    -> 3
                TV4F    -> 4
                TM22F   -> 4
                TM33F   -> 9
                TM44F   -> 16
            -- constant generic attribute
          constAttr -> GLSetVertexAttrib i constAttr
