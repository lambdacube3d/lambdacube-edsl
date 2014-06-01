module GamePhysics where

import Control.Monad
import Physics.Bullet.Raw
import Physics.Bullet.Raw.Class
import Physics.Bullet.Raw.Types
import Physics.Bullet.Raw.Utils

import LC_Mesh
import BulletUtil

{-
	enum	DebugDrawModes
	{
		DBG_NoDebug=0,
		DBG_DrawWireframe = 1,
		DBG_DrawAabb=2,
		DBG_DrawFeaturesText=4,
		DBG_DrawContactPoints=8,
		DBG_NoDeactivation=16,
		DBG_NoHelpText = 32,
		DBG_DrawText=64,
		DBG_ProfileTimings = 128,
		DBG_EnableSatComparison = 256,
		DBG_DisableBulletLCP = 512,
		DBG_EnableCCD = 1024,
		DBG_DrawConstraints = (1 << 11),
		DBG_DrawConstraintLimits = (1 << 12),
		DBG_FastWireframe = (1<<13),
		DBG_MAX_DEBUG_DRAW_MODE
	};
-}
mkPhysicsWorld :: IO BtDiscreteDynamicsWorld
--mkPhysicsWorld :: IO BtContinuousDynamicsWorld
mkPhysicsWorld = do
    -- create physics world
    --dynamicsWorld <- simpleBtContinuousDynamicsWorldM
    dynamicsWorld <- simpleBtDiscreteDynamicsWorldM

    -- setup debug drawer
    debugDrawer <- btGLDebugDrawer
    btIDebugDraw_setDebugMode debugDrawer (1+4+64)
    btCollisionWorld_setDebugDrawer dynamicsWorld debugDrawer
    return dynamicsWorld

addStaticPlane :: BtDynamicsWorldClass bc => bc -> Vec3 -> Float -> Float -> Float -> IO ()
addStaticPlane dynamicsWorld p0 dist friction restitution = do
    shape <- btStaticPlaneShape p0 dist
    motionState <- btDefaultMotionState idTransform idTransform
    body <- btRigidBody1 0 motionState shape zero
    --btCollisionObject_setFriction body friction
    --btCollisionObject_setRestitution body restitution
    btDynamicsWorld_addRigidBody dynamicsWorld body

addStaticShape :: BtDynamicsWorldClass bc => bc -> [Mesh] -> Float -> Float -> IO ()
addStaticShape dynamicsWorld mesh friction restitution = do
    shape <- mkStaticTriangleMeshShape mesh
    motionState <- btDefaultMotionState idTransform idTransform
    body <- btRigidBody1 0 motionState shape zero
    --btCollisionObject_setFriction body friction
    --btCollisionObject_setRestitution body restitution
    btDynamicsWorld_addRigidBody dynamicsWorld body

createCar :: BtDynamicsWorldClass bc => bc -> [Mesh] -> [(Vec3,Float,Float,[Mesh])] -> Proj4 -> IO BtRaycastVehicle
createCar dynamicsWorld carChassisMesh wheels transform = do
    carChassisShape <- mkConvexTriangleMeshShape carChassisMesh
    (_carMotionState,carChassisBody,carVehicle) <- mkVehicle dynamicsWorld carChassisShape 800 wheels
    btRigidBody_proceedToTransform carChassisBody $ proj4ToTransform transform
    return carVehicle

updateCar :: BtRaycastVehicleClass bc => bc -> IO [Proj4]
updateCar carVehicle = forM [0..3] $ \i -> do
    btRaycastVehicle_updateWheelTransform carVehicle i True
    wi <- btRaycastVehicle_getWheelInfo carVehicle i
    wt <- btWheelInfo_m_worldTransform_get wi
    return $ transformToProj4 wt
    --s <- btRaycastVehicle_getCurrentSpeedKmHour carVehicle
    --putStrLn $ "car speed: " ++ show s

addCar :: (BtDynamicsWorldClass bc, BtRaycastVehicleClass v) => bc -> v -> IO ()
addCar dw v = do
  btDynamicsWorld_addRigidBody dw =<< btRaycastVehicle_getRigidBody v
  btDynamicsWorld_addVehicle dw v

removeCar :: (BtDynamicsWorldClass bc, BtRaycastVehicleClass v) => bc -> v -> IO ()
removeCar dw v = do
  btDynamicsWorld_removeVehicle dw v
  btDynamicsWorld_removeRigidBody dw =<< btRaycastVehicle_getRigidBody v

getCarMotionState :: (BtRaycastVehicleClass v) => v -> IO BtMotionState
getCarMotionState = btRigidBody_getMotionState <=< btRaycastVehicle_getRigidBody

setCarMotionState :: (BtRaycastVehicleClass v) => v -> BtMotionState -> IO ()
setCarMotionState v s = do
  b <- btRaycastVehicle_getRigidBody v
  btRigidBody_setMotionState b s

steerCar :: BtRaycastVehicleClass bc => Float -> bc -> [Bool] -> IO ()
steerCar dt carVehicle [left,up,down,right,restore] = do
    when restore $ do
        carBody <- btRaycastVehicle_getRigidBody carVehicle
        btRigidBody_applyTorque carBody (Vec3 30000 30000 30000)
        return ()

    let (gEngineForce, gBrakingForce) = case (up,down) of
            (False,True)    -> (0,5*120)
            (True,False)    -> (15*120,0)
            _               -> (0,0)
    btRaycastVehicle_applyEngineForce carVehicle gEngineForce 2
    btRaycastVehicle_setBrake carVehicle gBrakingForce 2

    btRaycastVehicle_applyEngineForce carVehicle gEngineForce 3
    btRaycastVehicle_setBrake carVehicle gBrakingForce 3

    steering <- btRaycastVehicle_getSteeringValue carVehicle 0
    let fi = 1.2 * dt
        steering' = case (left,right) of
            (False,True) -> max (-0.45) (steering-fi)
            (True,False) -> min   0.45  (steering+fi)
            _            -> case steering > 0 of
                True  -> max 0 (steering-fi)
                False -> min 0 (steering+fi)
    btRaycastVehicle_setSteeringValue carVehicle steering' 0
    btRaycastVehicle_setSteeringValue carVehicle steering' 1

rigidBodyProj4 :: BtRigidBody -> IO Proj4
rigidBodyProj4 rigidBody = do
    motionState <- btRigidBody_getMotionState rigidBody
    t <- btMotionState_getWorldTransform motionState idTransform
    return $ transformToProj4 t

proj4ToTransform :: Proj4 -> Transform
proj4ToTransform p = Transform (Mat3 (Vec3 a1 a2 a3) (Vec3 b1 b2 b3) (Vec3 c1 c2 c3)) (Vec3 p1 p2 p3)
  where
    Mat4 (Vec4 a1 b1 c1 _) (Vec4 a2 b2 c2 _) (Vec4 a3 b3 c3 _) (Vec4 p1 p2 p3 _) = fromProjective p

transformToProj4 :: Transform -> Proj4
transformToProj4 t = toProjectiveUnsafe $ Mat4 (Vec4 a1 b1 c1 0) (Vec4 a2 b2 c2 0) (Vec4 a3 b3 c3 0) (Vec4 p1 p2 p3 1)
  where
    Transform (Mat3 (Vec3 a1 a2 a3) (Vec3 b1 b2 b3) (Vec3 c1 c2 c3)) (Vec3 p1 p2 p3) = t

mkVehicle :: (BtDynamicsWorldClass bc,  BtCollisionShapeClass p1)
          => bc
          -> p1
          -> Float
          -> [(Vec3,Float,Float,[Mesh])]
          -> IO (BtDefaultMotionState, BtRigidBody, BtRaycastVehicle)
mkVehicle dw chassisShape mass wheels = do
    compound <- btCompoundShape True
    let localTrans = Transform idmtx $ Vec3 0 0 0
    btCompoundShape_addChildShape compound localTrans chassisShape

    (carMotionSate,carChassis) <- localCreateRigidBodyM dw mass idTransform compound
    --(carMotionSate,carChassis) <- localCreateRigidBody dw 8 (Transform idmtx $ Vector3 480.0 20.3 (-520.0)) compound
    --wheelShape <- btCylinderShapeX $ Vector3 wheelWidth wheelRadius wheelRadius
    btRigidBody_setCenterOfMassTransform carChassis idTransform
    btRigidBody_setLinearVelocity carChassis zero
    btRigidBody_setAngularVelocity carChassis zero

    tuning <- btRaycastVehicle_btVehicleTuning
    vehicleRayCaster <- btDefaultVehicleRaycaster dw
    vehicle <- btRaycastVehicle tuning carChassis vehicleRayCaster
    btCollisionObject_setActivationState carChassis 4 -- #define DISABLE_DEACTIVATION 4

    btRaycastVehicle_setCoordinateSystem vehicle 0 1 2
    let wheelDirectionCS0       = Vec3 0 (-1) 0
        wheelAxleCS             = Vec3 (-1) 0 0
        suspensionRestLength    = 0.1
        suspensionStiffness     = 40
        suspensionDamping       = 2.3
        suspensionCompression   = 1.4
        rollInfluence           = 0.2
        wheelFriction           = 3
        
        m_maxSuspensionTravelCm     = 20
        m_maxSuspensionForce        = 6000

    forM_ wheels $ \(Vec3 x y z,w,r,_) -> btRaycastVehicle_addWheel vehicle (Vec3 x y (-z)) wheelDirectionCS0 wheelAxleCS suspensionRestLength r tuning True
    numWheels <- btRaycastVehicle_getNumWheels vehicle
    forM_ [0..numWheels-1] $ \i -> do
        wheel <- btRaycastVehicle_getWheelInfo vehicle i
        when (i < 2) $ btWheelInfo_m_bIsFrontWheel_set wheel False
        btWheelInfo_m_suspensionStiffness_set wheel suspensionStiffness
        btWheelInfo_m_wheelsDampingRelaxation_set wheel suspensionDamping
        btWheelInfo_m_wheelsDampingCompression_set wheel suspensionCompression
        btWheelInfo_m_frictionSlip_set wheel wheelFriction
        btWheelInfo_m_rollInfluence_set wheel rollInfluence
        btWheelInfo_m_maxSuspensionTravelCm_set wheel m_maxSuspensionTravelCm
        btWheelInfo_m_maxSuspensionForce_set wheel m_maxSuspensionForce
    {-
    forM_ [0..numWheels-1] $ \i -> do
        wheel <- btRaycastVehicle_getWheelInfo vehicle i
        print =<< btWheelInfo_m_suspensionStiffness_get wheel
        print =<< btWheelInfo_m_wheelsDampingRelaxation_get wheel
        print =<< btWheelInfo_m_wheelsDampingCompression_get wheel
        print =<< btWheelInfo_m_frictionSlip_get wheel
        print =<< btWheelInfo_m_rollInfluence_get wheel
    -}
    removeCar dw vehicle
    return (carMotionSate,carChassis,vehicle)
