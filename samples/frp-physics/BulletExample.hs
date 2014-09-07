{-# LANGUAGE OverloadedStrings, TypeOperators, NoMonomorphismRestriction, ExistentialQuantification, PackageImports, DoRec, ParallelListComp, DataKinds #-}

{-

Notes:

* btCollisionShape can be handled as an immutable, shareable object,
  so functions creating any of its descendants don't have to be in IO

* access to all constants would be nice (e.g. no activation states available)

* the physics world could be just as well made accessible from a monad as the LC world

* how to check if a boxed pointer is null?

-}

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.Maybe
import Data.List
import qualified Data.Trie as T
import Data.Vect
import Data.Vector ((!))
import qualified Data.Vector as V 
import Foreign hiding (unsafePerformIO)
import FRP.Elerea.Simple
import "GLFW-b" Graphics.UI.GLFW
import Physics.Bullet.Raw
import Physics.Bullet.Raw.Class
import Physics.Bullet.Raw.Types
import Physics.Bullet.Raw.Utils
import System.IO.Unsafe
import Unsafe.Coerce

import LambdaCube.GL hiding (Transform)
import qualified LambdaCube.GL as LC
import LambdaCube.GL.Mesh

import Utils
import GraphicsUtils

data CameraInfo = CameraInfo
    { cameraPosition :: Vec3
    , targetPosition :: Vec3
    , upwardDirection :: Vec3
    }

cameraView (CameraInfo cameraPos targetPos upwardDir) = lookat cameraPos targetPos upwardDir
 
cameraInfo = CameraInfo (Vec3 0 20 30) (Vec3 0 0 0) (Vec3 0 1 0)

farPlane = 5000

fieldOfView = pi / 2

floorSize = Vec3 100.0 1.0 100.0

brickSize = Vec3 5.0 2.0 10.0

ghostRadius = 5

capsuleBoxSize radius height = Vec3 radius (height/2+radius) radius

pi2 = pi*0.5

pi4 = pi*0.25

scaleTransPos m (Transform rot pos) = Transform rot (pos &* m)

--ragdollPartConfig :: [(String, (Float, Float, Transform))]
ragdollPartConfig = map (scalePart 5)
    [ ("Pelvis", (0.15, 0.20, Transform idmtx (Vec3 0 1 0)))
	, ("Spine", (0.15, 0.28, Transform idmtx (Vec3 0 1.2 0)))
	, ("Head", (0.10, 0.05, Transform idmtx (Vec3 0 1.6 0)))
	, ("LeftUpperLeg", (0.07, 0.45, Transform idmtx (Vec3 (-0.18) 0.65 0)))
	, ("LeftLowerLeg", (0.05, 0.37, Transform idmtx (Vec3 (-0.18) 0.2 0)))
	, ("RightUpperLeg", (0.07, 0.45, Transform idmtx (Vec3 0.18 0.65 0)))
	, ("RightLowerLeg", (0.05, 0.37, Transform idmtx (Vec3 0.18 0.2 0)))
	, ("LeftUpperArm", (0.05, 0.33, Transform (rotMatrixZ pi2) (Vec3 (-0.35) 1.45 0)))
	, ("LeftLowerArm", (0.04, 0.25, Transform (rotMatrixZ pi2) (Vec3 (-0.7) 1.45 0)))
	, ("RightUpperArm", (0.05, 0.33, Transform (rotMatrixZ (-pi/2)) (Vec3 0.35 1.45 0)))
	, ("RightLowerArm", (0.04, 0.25, Transform (rotMatrixZ (-pi/2)) (Vec3 0.7 1.45 0)))
    ]
  where
    scalePart m (name, (radius, height, trans)) = (name, (radius*m, height*m, scaleTransPos m trans))

ragdollConstraintConfig = map (scaleConstraint 5)
    [ HingeConstraint "Pelvis" "Spine" (Transform (rotMatrixY pi2) (Vec3 0 0.15 0)) (Transform (rotMatrixY pi2) (Vec3 0 (-0.15) 0)) (-pi4) pi2
    , ConeTwistConstraint "Spine" "Head" (Transform (rotMatrixZ pi2) (Vec3 0 0.3 0)) (Transform (rotMatrixZ pi2) (Vec3 0 (-0.14) 0)) pi4 pi4 pi2
    , hipConstraint "LeftUpperLeg" (-1)
    , kneeConstraint "LeftUpperLeg" "LeftLowerLeg"
    , hipConstraint "RightUpperLeg" 1
    , kneeConstraint "RightUpperLeg" "RightLowerLeg"
    , shoulderConstraint "LeftUpperArm" (-1)
    , elbowConstraint "LeftUpperArm" "LeftLowerArm"
    , shoulderConstraint "RightUpperArm" 1
    , elbowConstraint "RightUpperArm" "RightLowerArm"
    ]
  where
    hipConstraint upperLeg sign = ConeTwistConstraint "Pelvis" upperLeg (Transform (rotMatrixZ (sign*pi4)) (Vec3 (sign*0.18) (-0.1) 0)) (Transform (rotMatrixZ (sign*pi4)) (Vec3 0 0.225 0)) pi4 pi4 0
    kneeConstraint upperLeg lowerLeg = HingeConstraint upperLeg lowerLeg (Transform (rotMatrixY pi2) (Vec3 0 (-0.225) 0)) (Transform (rotMatrixY pi2) (Vec3 0 0.185 0)) 0 pi2
    shoulderConstraint upperArm sign = ConeTwistConstraint "Spine" upperArm (Transform (rotMatrixZ (pi2-sign*pi2)) (Vec3 (sign*0.2) 0.15 0)) (Transform (rotMatrixZ pi2) (Vec3 0 (-0.18) 0)) pi2 pi2 0
    elbowConstraint upperArm lowerArm = HingeConstraint upperArm lowerArm (Transform (rotMatrixY pi2) (Vec3 0 0.18 0)) (Transform (rotMatrixY pi2) (Vec3 0 (-0.14) 0)) 0 pi2
    scaleConstraint m c = case c of
        HingeConstraint name1 name2 trans1 trans2 low high -> HingeConstraint name1 name2 (sc trans1) (sc trans2) low high
        ConeTwistConstraint name1 name2 trans1 trans2 swingSpan1 swingSpan2 twistSpan -> ConeTwistConstraint name1 name2 (sc trans1) (sc trans2) swingSpan1 swingSpan2 twistSpan
      where
        sc = scaleTransPos m
        
-- This is missing a lot of stuff (including other constructors?), it should be handled by attributes
data BulletConstraint
    = HingeConstraint String String Transform Transform Float Float
    | ConeTwistConstraint String String Transform Transform Float Float Float

-- Capsules only...
complexBody dynamicsWorld offset parts constraints = do
    bodies <- forM parts $ \(name, (radius, height, Transform rot pos)) -> do
        body <- snd <$> localCreateRigidBodyM dynamicsWorld 1 (Transform rot (pos &+ offset)) (capsuleShape radius height)
        return (name, body)
    let body name = snd (fromJust (find ((==name) . fst) bodies))
    forM_ constraints $ \ctr -> case ctr of
        HingeConstraint name1 name2 trans1 trans2 low high -> do
            hinge <- btHingeConstraint2 (body name1) (body name2) trans1 trans2 False
            btHingeConstraint_setLimit hinge low high 0.9 0.3 1
            --set hinge [lowerLimit? := low, upperLimit? := high, limitSoftness := 0.9, biasFactor := 0.3, relaxationFactor := 1]
            btDynamicsWorld_addConstraint dynamicsWorld hinge True
        ConeTwistConstraint name1 name2 trans1 trans2 swSpan1 swSpan2 twSpan -> do
            coneTwist <- btConeTwistConstraint0 (body name1) (body name2) trans1 trans2
            --set coneTwist [swingSpan1 := swSpan1, swingSpan2 := swSpan2, twistSpan := twSpan, limitSoftness := 1, biasFactor := 0.3, relaxationFactor := 1]
            btConeTwistConstraint_setLimit1 coneTwist swSpan1 swSpan2 twSpan 1 0.3 1
            btDynamicsWorld_addConstraint dynamicsWorld coneTwist True
    return bodies    

-- Attribute system in the footsteps of gtk2hs (glib)

infixr 0 :=, :~, :<

data Attr o a = forall x . Attr !(o -> IO a) !(o -> a -> IO x)

data AttrOp o = forall a . Attr o a := a
              | forall a . Attr o a :~ (a -> a)
              | forall a . Attr o a :!= IO a
              | forall a . Attr o a :!~ (a -> IO a)
              | forall a . Attr o a :< Signal (Maybe a)
  
set :: o -> [AttrOp o] -> IO o
set obj attrs = (>> return obj) $ forM_ attrs $ \op -> case op of
    Attr _ setter := x       -> setter obj x >> return ()
    Attr getter setter :~ f  -> getter obj >>= setter obj . f >> return ()
    Attr _ setter :!= x      -> x >>= setter obj >> return ()
    Attr getter setter :!~ f -> getter obj >>= f >>= setter obj >> return ()
    _ :< _                   -> error "Signals not supported in IO"

get :: o -> Attr o a -> IO a
get obj (Attr getter _) = getter obj 

make :: IO o -> [AttrOp o] -> IO o
make act flags = do
    obj <- act
    set obj flags
    return obj

set' :: o -> [AttrOp o] -> SignalGen (Signal ())
set' obj as = go as (return ())
  where
    go [] sig     = return sig
    go (a:as) sig = case a of
        Attr getter setter := x  -> execute (setter obj x >> return ()) >> go as sig
        Attr getter setter :~ f  -> execute (getter obj >>= setter obj . f >> return ()) >> go as sig
        Attr getter setter :!= x -> execute (x >>= setter obj >> return ()) >> go as sig
        Attr getter setter :!~ f -> execute (getter obj >>= f >>= setter obj >> return ()) >> go as sig
        Attr getter setter :< s  -> do     
            dummy <- flip effectful1 s $ \mx -> case mx of
                Nothing -> return ()
                Just x  -> setter obj x >> return ()
            go as (liftA2 const sig dummy)

make' :: IO o -> [AttrOp o] -> SignalGen (Signal o)
make' act flags = do
    obj <- execute act
    dummy <- set' obj flags
    return (liftA2 const (return obj) dummy)

-- Test attributes

collisionFlags :: BtCollisionObjectClass o => Attr o Int
collisionFlags = Attr btCollisionObject_getCollisionFlags btCollisionObject_setCollisionFlags

-- coercion needed to generalise concrete type into a vague type class (should be safe)
collisionShape :: (BtCollisionObjectClass o, BtCollisionShapeClass cs) => Attr o cs
collisionShape = Attr (unsafeCoerce . btCollisionObject_getCollisionShape) btCollisionObject_setCollisionShape

worldTransform :: BtCollisionObjectClass o => Attr o Transform
worldTransform = Attr btCollisionObject_getWorldTransform btCollisionObject_setWorldTransform

deactivationTime :: BtCollisionObjectClass o => Attr o Float
deactivationTime = Attr btCollisionObject_getDeactivationTime btCollisionObject_setDeactivationTime

-- note the inconsistent naming convention...
pivotA :: BtPoint2PointConstraintClass o => Attr o Vec3
pivotA = Attr btPoint2PointConstraint_getPivotInA btPoint2PointConstraint_setPivotA

pivotB :: BtPoint2PointConstraintClass o => Attr o Vec3
pivotB = Attr btPoint2PointConstraint_getPivotInB btPoint2PointConstraint_setPivotB

-- it would be great if all the constraints provided a similar facility
setting :: BtPoint2PointConstraintClass o => Attr o BtConstraintSetting
setting = Attr btPoint2PointConstraint_m_setting_get btPoint2PointConstraint_m_setting_set

impulseClamp :: BtConstraintSettingClass o => Attr o Float
impulseClamp = Attr btConstraintSetting_m_impulseClamp_get btConstraintSetting_m_impulseClamp_set

tau :: BtConstraintSettingClass o => Attr o Float
tau = Attr btConstraintSetting_m_tau_get btConstraintSetting_m_tau_set

damping :: BtConstraintSettingClass o => Attr o Float
damping = Attr btConstraintSetting_m_damping_get btConstraintSetting_m_damping_set

-- Collision tracking example

extractManifold :: BtPersistentManifold -> IO (BtCollisionObject,BtCollisionObject,[(Float,Vec3,Vec3,Vec3)])
extractManifold manifold = do
    b0 <- mkBtCollisionObject =<< btPersistentManifold_getBody0 manifold
    b1 <- mkBtCollisionObject =<< btPersistentManifold_getBody1 manifold
    cpn <- btPersistentManifold_getNumContacts manifold
    l <- forM [0..cpn-1] $ \p -> do
        pt <- btPersistentManifold_getContactPoint manifold p
        (,,,) <$>
            btManifoldPoint_getDistance pt <*> btManifoldPoint_getPositionWorldOnA pt <*> 
            btManifoldPoint_getPositionWorldOnB pt <*> btManifoldPoint_m_normalWorldOnB_get pt
    return (b0,b1,l)

collectManifolds :: BtCollisionWorldClass cw => cw -> BtPairCachingGhostObject -> IO [BtPersistentManifold]
collectManifolds dynamicsWorld ghostObject = do
    let notNull a = btToPtr a /= nullPtr
    manifoldArray <- btAlignedObjectArray_btPersistentManifold_ptr_
    pairArray <- btHashedOverlappingPairCache_getOverlappingPairArray =<< btPairCachingGhostObject_getOverlappingPairCache ghostObject
    numPairs <- btAlignedObjectArray_btBroadphasePair__size pairArray
    l <- forM [0..numPairs-1] $ \i -> do
        btAlignedObjectArray_btPersistentManifold_ptr__clear manifoldArray
        pair <- btAlignedObjectArray_btBroadphasePair__at pairArray i
        pProxy0 <- btBroadphasePair_m_pProxy0_get pair
        pProxy1 <- btBroadphasePair_m_pProxy1_get pair
        collisionPair <- (\a -> btOverlappingPairCache_findPair a pProxy0 pProxy1) =<< btCollisionWorld_getPairCache dynamicsWorld
        case notNull collisionPair of
            False -> return []
            True -> do
                alg <- btBroadphasePair_m_algorithm_get collisionPair
                when (notNull alg) $ btCollisionAlgorithm_getAllContactManifolds alg manifoldArray
                n <- btAlignedObjectArray_btPersistentManifold_ptr__size manifoldArray
                forM [0..n-1] $ \j -> btAlignedObjectArray_btPersistentManifold_ptr__at manifoldArray j
    --btAlignedObjectArray_btPersistentManifold_ptr__free manifoldArray
    return $ concat l

rigidBodyProj4 :: BtRigidBody -> IO Proj4
rigidBodyProj4 rigidBody = do
    motionState <- btRigidBody_getMotionState rigidBody
    t <- btMotionState_getWorldTransform motionState idTransform
    return (transformToProj4 t)

proj4ToTransform :: Proj4 -> Transform
proj4ToTransform p = Transform (Mat3 (Vec3 a1 a2 a3) (Vec3 b1 b2 b3) (Vec3 c1 c2 c3)) (Vec3 p1 p2 p3)
  where
    Mat4 (Vec4 a1 b1 c1 _) (Vec4 a2 b2 c2 _) (Vec4 a3 b3 c3 _) (Vec4 p1 p2 p3 _) = fromProjective p

transformToProj4 :: Transform -> Proj4
transformToProj4 t = toProjectiveUnsafe $ Mat4 (Vec4 a1 b1 c1 0) (Vec4 a2 b2 c2 0) (Vec4 a3 b3 c3 0) (Vec4 p1 p2 p3 1)
  where
    Transform (Mat3 (Vec3 a1 a2 a3) (Vec3 b1 b2 b3) (Vec3 c1 c2 c3)) (Vec3 p1 p2 p3) = t

main' = do
    dynamicsWorld <- simpleBtDiscreteDynamicsWorldM

    -- setup
    ghostPairCallback <- btGhostPairCallback
    pairCache <- btCollisionWorld_getPairCache dynamicsWorld
    btOverlappingPairCache_setInternalGhostPairCallback pairCache ghostPairCallback

    ghostObject <- btPairCachingGhostObject

    sphere <- btSphereShape 5
    print sphere
    print (btToPtr ghostObject < btToPtr sphere)
    btCollisionObject_setCollisionShape ghostObject sphere
    btCollisionObject_setWorldTransform ghostObject $ Transform idmtx $ Vec3 0 5 0
    btCollisionWorld_addCollisionObject dynamicsWorld ghostObject 1 (-1)

    (_,b) <- localCreateRigidBodyM dynamicsWorld 1 (Transform idmtx $ Vec3 0 6 0) sphere
    print (ghostObject,b)

    print =<< mapM extractManifold =<< collectManifolds dynamicsWorld ghostObject

    -- ray test
    let from = Vec3 0 100 0
        to = Vec3 0 (-100) 0
    rayResult <- btCollisionWorld_AllHitsRayResultCallback from to
    btCollisionWorld_rayTest dynamicsWorld from to rayResult
    l <- btCollisionWorld_AllHitsRayResultCallback_m_hitPointWorld_get rayResult
    -- m_collisionObjects: btAlignedObjectArray<btCollisionObject*>
    -- btAlignedObjectArray_btCollisionObject_ptr__at
    n <- btAlignedObjectArray_btVector3__size l
    hitPoints <- forM [0..n-1] $ \i -> btAlignedObjectArray_btVector3__at l i
    print ("ray test",hitPoints)

    -- ghost object collision test
    btDynamicsWorld_stepSimulation dynamicsWorld 0.01 10 (1 / 200)
    print =<< (mapM extractManifold =<< collectManifolds dynamicsWorld ghostObject)
    btDynamicsWorld_stepSimulation dynamicsWorld 10 10 (1 / 200)
    print =<< (mapM extractManifold =<< collectManifolds dynamicsWorld ghostObject)

sphereShape = unsafePerformIO . btSphereShape

boxShape = unsafePerformIO . btBoxShape

capsuleShape r h = unsafePerformIO (btCapsuleShape1 r h)

bodyTransformation = effectful1 rigidBodyProj4

boolToMaybe val bool = if bool then Just val else Nothing

main = do
    (windowSize, mousePosition, mousePress) <- initCommon "LambdaCube-Bullet test"

    dynamicsWorld <- simpleBtDiscreteDynamicsWorldM
    ghostPairCallback <- btGhostPairCallback
    pairCache <- btCollisionWorld_getPairCache dynamicsWorld
    btOverlappingPairCache_setInternalGhostPairCallback pairCache ghostPairCallback
  
    let stepPhysics dt = btDynamicsWorld_stepSimulation dynamicsWorld dt 50 0.005
        collisionInfo gobj = mapM extractManifold =<< collectManifolds dynamicsWorld gobj
        bodyInCollision body = not . null . filter (involves body)
          where
            involves b (b1,b2,_) = b == unsafeCoerce b1 || b == unsafeCoerce b2

    let pipeline :: Exp Obj (Image 1 V4F)
        pipeline = PrjFrameBuffer "outFB" tix0 translucentShading --simpleShading
    
    (duration, renderer) <- measureDuration $ compileRenderer (ScreenOut pipeline)
    putStrLn $ "Renderer compiled - " ++ show duration
    
    let setters = uniformSetter renderer
        lightPositionSetter = uniformV3F "lightPosition" setters . fromVec3

    lightPositionSetter (Vec3 10 10 10)
    
    let createObject name mesh colour = do
            let (slotName, uniformName) = case colour of 
                    Left _ -> ("solidGeometry", "solidColour")
                    Right _ -> ("translucentGeometry", "alphaColour")

            compiledMesh <- compileMesh mesh
            object <- addMesh renderer slotName compiledMesh [uniformName, "modelMatrix"]
            let objectSetters = objectUniformSetter object
                modelMatrixSetter = uniformM44F "modelMatrix" objectSetters . fromMat4
            
            modelMatrixSetter idmtx
            case colour of
                Left rgb -> uniformV3F uniformName objectSetters (fromVec3 rgb)
                Right rgba -> uniformV4F uniformName objectSetters (fromVec4 rgba)
            return (name, modelMatrixSetter)

    ghostSetter <- createObject "Ghost" (sphere 5 10) (Right (Vec4 0.3 0.9 0.9 0.7))
    floorSetter <- createObject "Floor" (box floorSize) (Left (Vec3 0.7 0.7 0.7))
    brickSetter <- createObject "Brick" (box brickSize) (Left (Vec3 1.0 0.0 0.0))
    hitSetter <- createObject "Hit" (sphere 0.5 5) (Right (Vec4 1.0 1.0 1.0 0.7))

    ragdollSetters <- forM ragdollPartConfig $ \(name, (radius, height, trans)) -> do
        ragdollSetter@(_, setTrans) <- createObject name (capsule radius height 10) (Left (Vec3 1.0 0.9 0.6))
        setTrans (fromProjective (transformToProj4 trans))
        return ragdollSetter

    let updateTransforms transforms = forM_ transforms $ \(name, trans) -> do
            let Just setter = T.lookup (B.pack name) setters
            setter (fromProjective trans)
        setters = T.fromList (map (first B.pack) namedSetters)  
          where
            namedSetters = ghostSetter : floorSetter : brickSetter : hitSetter : ragdollSetters

    smp <- start $ do
        ragdollBodies <- execute $ do
            floor <- localCreateRigidBodyM dynamicsWorld 0 (Transform idmtx 0) (boxShape floorSize)
            complexBody dynamicsWorld (Vec3 1 5 10) ragdollPartConfig ragdollConstraintConfig

        querySpace <- execute $ do
            ghostObject <- make btPairCachingGhostObject
                           [ collisionFlags :~ (.|. e_btCollisionObject_CollisionFlags_CF_NO_CONTACT_RESPONSE)
                           , collisionShape := sphereShape ghostRadius
                           , worldTransform := Transform idmtx 0
                           ]
            btCollisionWorld_addCollisionObject dynamicsWorld ghostObject 1 (-1)
            return ghostObject

        collisions <- effectful $ collisionInfo querySpace

        let initBrickTrans = Transform idmtx (Vec3 2 20 (-3))
        brick <- do
            rec brick <- make' (snd <$> localCreateRigidBodyM dynamicsWorld 1 initBrickTrans (boxShape brickSize))
                         [worldTransform :< boolToMaybe initBrickTrans . bodyInCollision brickBody <$> collisions]
                brickBody <- snapshot brick
            return brick
    
        brickTrans <- bodyTransformation brick
        
        (hitPosition, hitPositionSink) <- execute $ external Nothing
    
        dummy <- pickConstraint dynamicsWorld windowSize (pure cameraInfo) mousePress mousePosition hitPositionSink

        return $ updateScene renderer updateTransforms stepPhysics ragdollBodies <$> windowSize <*> hitPosition <*> (const <$> brickTrans <*> dummy)
  
    fix $ \loop -> do
        join smp
        esc <- keyIsPressed KeyEsc
        when (not esc) loop
    
    dispose renderer
    putStrLn "Renderer destroyed."

    closeWindow

updateScene :: Renderer -> ([(String, Proj4)] -> IO ()) -> (Float -> IO Int) -> [(String, BtRigidBody)] -> Vec2 -> Maybe Vec3 -> Proj4 -> IO ()
updateScene renderer updateTransforms stepPhysics ragdollBodies (Vec2 w h) hitPosition brickTrans = do
    ragdollTransforms <- forM ragdollBodies $ \(name, body) -> do
        proj <- rigidBodyProj4 body
        return (name, proj)
    
    let aspect = w / h
        cameraProjection = perspective 0.1 farPlane fieldOfView aspect
        cameraSetter = uniformM44F "cameraMatrix" (uniformSetter renderer) . fromMat4
    setScreenSize renderer (floor w) (floor h)
    cameraSetter $ fromProjective (cameraView cameraInfo) .*. cameraProjection

    updateTransforms $
        ("Brick", brickTrans) :
        ("Hit", translation (fromMaybe (Vec3 0 10000 0) hitPosition)) :
        ragdollTransforms
    
    dt <- getTime
    resetTime
    stepPhysics (realToFrac dt)
    
    render renderer
    swapBuffers

-- Picking

pickConstraint dynamicsWorld windowSize cameraInfo mouseButton mousePos hitPositionSink = do
    press <- edge mouseButton
    release <- edge (not <$> mouseButton)
    pick <- generator $ makePick <$> press <*> windowSize <*> cameraInfo <*> mousePos
    
    -- We're going to all this trouble just to keep a reference to the
    -- constraint signal: we sample it in every frame even though it
    -- is constant
    releaseInfo <- do
        rec sig <- delay Nothing $ do
                released <- release
                newPick <- pick
                currentPick <- sig
                case (released, newPick, currentPick) of
                    (True, _, _)                             -> return Nothing
                    (_, Just (constraintSignal, body), _)    -> do
                        constraint <- constraintSignal
                        return $ Just (constraint, body, constraintSignal)
                    (_, _, Just (_, body, constraintSignal)) -> do
                        constraint <- constraintSignal
                        return $ Just (constraint, body, constraintSignal)
                    _                                        -> return Nothing                            

        return sig
    
    effectful2 stopPicking release releaseInfo
  where
    edge sig = do
        sig' <- delay False sig
        return $ do
            cur <- sig
            prev <- sig'
            return $ not prev && cur 
    
    stopPicking True (Just (constraint, body, _)) = releasePick dynamicsWorld body constraint
    stopPicking _ _ = return ()
    
    makePick press windowSizeCur cameraInfoCur mousePosCur = case press of
        False -> return Nothing
        True -> do
            pickInfo <- execute $ pickBody dynamicsWorld windowSizeCur cameraInfoCur mousePosCur hitPositionSink
            case pickInfo of
                Nothing -> return Nothing
                Just (body, hitPosition, distance) -> do
                    constraint <- createPick dynamicsWorld body hitPosition distance windowSize cameraInfo mousePos
                    return $ Just (constraint, body)
    
-- body picked, ray hit position, and distance from the camera at the time of picking (to be kept while moving)
--pickBody :: BtCollisionWorldClass bc => bc -> Vec2 -> CameraInfo -> Vec2-> (Maybe Vec3 -> IO ()) -> IO (Maybe (BtRigidBody, Vec3, Float))
pickBody dynamicsWorld windowSize cameraInfo mousePos hitPositionSink = do
    let rayFrom = cameraPosition cameraInfo
        rayTo = rayTarget windowSize cameraInfo mousePos
    rayResult <- btCollisionWorld_ClosestRayResultCallback rayFrom rayTo
    btCollisionWorld_rayTest dynamicsWorld rayFrom rayTo rayResult
    hasHit <- btCollisionWorld_RayResultCallback_hasHit rayResult
    
    case hasHit of
        False -> do
            hitPositionSink Nothing
            return Nothing
        True -> do
            collisionObj <- btCollisionWorld_RayResultCallback_m_collisionObject_get rayResult
            isNotPickable <- btCollisionObject_isStaticOrKinematicObject collisionObj
            hitPositionSink =<< Just <$> btCollisionWorld_ClosestRayResultCallback_m_hitPointWorld_get rayResult
            internalType <- btCollisionObject_getInternalType collisionObj
            case isNotPickable || internalType /= e_btCollisionObject_CollisionObjectTypes_CO_RIGID_BODY of
                True -> return Nothing
                False -> do
                    btCollisionObject_setActivationState collisionObj 4 -- DISABLE_DEACTIVATION
                    hitPosition <- btCollisionWorld_ClosestRayResultCallback_m_hitPointWorld_get rayResult
                    body <- btRigidBody_upcast collisionObj -- this is null if the internal type is not CO_RIGID_BODY
                    return $ Just (body, hitPosition, len (hitPosition &- rayFrom))

createPick :: (BtDynamicsWorldClass bc, BtRigidBodyClass b)
           => bc -> b -> Vec3 -> Float -> Signal Vec2 -> Signal CameraInfo -> Signal Vec2 -> SignalGen (Signal BtPoint2PointConstraint)
createPick dynamicsWorld body hitPosition distance windowSize cameraInfo mousePos = do
    make' (createPickConstraint dynamicsWorld body hitPosition)
        [ setting :!~ flip set [impulseClamp := 30, tau := 0.001]
        , pivotB :< pivotPosition <$> windowSize <*> cameraInfo <*> mousePos
        ]
  where
    createPickConstraint dynamicsWorld body hitPosition = do
        bodyProj <- transformToProj4 <$> btRigidBody_getCenterOfMassTransform body
        let localPivot = trim ((extendWith 1 hitPosition :: Vec4) .* fromProjective (inverse bodyProj))
        pickConstraint <- btPoint2PointConstraint1 body localPivot
        btDynamicsWorld_addConstraint dynamicsWorld pickConstraint True
        return pickConstraint
        
    pivotPosition windowSize cameraInfo mousePos = Just (rayFrom &+ (normalize (rayTo &- rayFrom) &* distance))
      where
        rayFrom = cameraPosition cameraInfo
        rayTo = rayTarget windowSize cameraInfo mousePos

releasePick dynamicsWorld body constraint = do
    btDynamicsWorld_removeConstraint dynamicsWorld constraint
    btCollisionObject_forceActivationState body 1 -- ACTIVE_TAG
    btCollisionObject_setDeactivationTime body 0

rayTarget :: Vec2 -> CameraInfo -> Vec2 -> Vec3
rayTarget (Vec2 windowW windowH) (CameraInfo cameraPos targetPos cameraUp) (Vec2 windowX windowY) =
    rayCenter &- (horizontal &* (aspect*(0.5-windowX/windowW))) &+ (vertical &* (0.5-windowY/windowH))
  where
    aspect = windowW / windowH
    tanFov = tan (fieldOfView * sqrt 0.5)

    rayForward = normalize (targetPos &- cameraPos) &* farPlane
    horizontal = normalize (rayForward &^ cameraUp) &* (farPlane*tanFov)
    vertical = normalize (horizontal &^ rayForward) &* (farPlane*tanFov)
    
    rayCenter = cameraPos &+ rayForward

simpleShading :: Exp Obj (FrameBuffer 1 (Float, V4F))
simpleShading = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    accCtx = AccumulationContext Nothing (DepthOp Less True :. ColorOp NoBlending (one' :: V4B) :. ZT)
    clearBuf = FrameBuffer (DepthImage n1 1000 :. ColorImage n1 (V4 0 0 0 1) :. ZT)
    prims = LC.Transform vert (Fetch "solidGeometry" Triangles (IV3F "position", IV3F "normal"))
    
    cameraMatrix = Uni (IM44F "cameraMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")
    lightPosition = Uni (IV3F "lightPosition")
    colour = Uni (IV3F "solidColour")

    vert :: Exp V (V3F, V3F) -> VertexOut () (V3F, V3F)
    vert attr = VertexOut viewPos (floatV 1) ZT (Smooth (v4v3 worldPos) :. Smooth worldNormal :. ZT)
      where
        worldPos = modelMatrix @*. v3v4 localPos
        viewPos = cameraMatrix @*. worldPos
        worldNormal = normalize' (v4v3 (modelMatrix @*. n3v4 localNormal))
        (localPos, localNormal) = untup2 attr
        
    frag :: Exp F (V3F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag attr = FragmentOutRastDepth (v3v4 (colour @* light) :. ZT)
      where
        light = max' (floatF 0) (dot' worldNormal (normalize' (lightPosition @- worldPos)))
        (worldPos, worldNormal) = untup2 attr

translucentShading :: Exp Obj (FrameBuffer 1 (Float, V4F))
translucentShading = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) simpleShading
  where
    accCtx = AccumulationContext Nothing (DepthOp Less True :. ColorOp blending (one' :: V4B) :. ZT)
    blending = Blend (FuncAdd, FuncAdd) ((SrcAlpha, OneMinusSrcAlpha), (SrcAlpha, OneMinusSrcAlpha)) zero'
    prims = LC.Transform vert (Fetch "translucentGeometry" Triangles (IV3F "position", IV3F "normal"))
    
    cameraMatrix = Uni (IM44F "cameraMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")
    lightPosition = Uni (IV3F "lightPosition")
    colour = Uni (IV4F "alphaColour")

    vert :: Exp V (V3F, V3F) -> VertexOut () (V3F, V3F)
    vert attr = VertexOut viewPos (floatV 1) ZT (Smooth (v4v3 worldPos) :. Smooth worldNormal :. ZT)
      where
        worldPos = modelMatrix @*. v3v4 localPos
        viewPos = cameraMatrix @*. worldPos
        worldNormal = normalize' (v4v3 (modelMatrix @*. n3v4 localNormal))
        (localPos, localNormal) = untup2 attr
        
    frag :: Exp F (V3F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag attr = FragmentOutRastDepth (finalColour :. ZT)
      where
        V4 r g b a = unpack' colour
        finalColour = pack' (V4 (r @* light) (g @* light) (b @* light) a)
        light = max' (floatF 0) (dot' worldNormal (normalize' (lightPosition @- worldPos)))
        (worldPos, worldNormal) = untup2 attr

initCommon :: String -> IO (Signal Vec2, Signal Vec2, Signal Bool)
initCommon title = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_numRedBits         = 8
        , displayOptions_numGreenBits       = 8
        , displayOptions_numBlueBits        = 8
        , displayOptions_numAlphaBits       = 8
        , displayOptions_numDepthBits       = 24
        , displayOptions_width              = 1280
        , displayOptions_height             = 720
        , displayOptions_windowIsResizable  = True
        , displayOptions_openGLVersion      = (3,2)
        , displayOptions_openGLProfile      = CoreProfile
        }
    setWindowTitle title

    (windowSize, windowSizeSink) <- external (Vec2 0 0)
    setWindowSizeCallback $ \w h -> windowSizeSink (Vec2 (fromIntegral w) (fromIntegral h))
    
    (mousePosition, mousePositionSink) <- external (Vec2 0 0)
    setMousePositionCallback $ \x y -> mousePositionSink (Vec2 (fromIntegral x) (fromIntegral y))
    
    (mousePress, mousePressSink) <- external False
    setMouseButtonCallback $ \b p -> when (b == MouseButton0) $ mousePressSink p
    
    return (windowSize, mousePosition, mousePress)
