{-
  changes:
    unified multiplication operators (except for dot products)
    introduced record syntax
    shortened some constants (will probably need type directed resolution)
    clarified a few names

  todo:
    the interaction of interpolation specification with the record system (see vertex shader output)
-}
stuntsGFX :: FrameBuffer N1 (Float, V4F)
stuntsGFX = Accumulate accumContext (Filter isPartOfGrid) frag fragStream clearBuffer
  where
    const worldPosition :: M44F
          worldView :: M44F
          cameraProjection :: M44F
          lightPosition :: V3F
          lightViewProj :: M44F

    record InputGeometry =
        { position :: V3F
        , normal :: V3F
        , colour :: V4F
        , pattern :: Int
        , zBias :: Float
        , shininess :: Float
        }
    
    record ShadingInput =
        { lightViewPosition :: V4F
        , colour :: V4F
        , localPosition :: V3F
        , eyePosition :: V3F
        , eyeNormal :: V3F
        , pattern :: Int
        , zBias :: Int
        , shininess :: Float
        }
    
    stream streamSlot :: Triangle InputGeometry
    
    accumContext = {depthOp = (Less, Write), colorOp = (NoBlending, [True, True, True, True])}
    rasterContext = TriangleContext {cullMode = None, polygonMode = Fill, polygonOffset = None, provokingVertex = Last}
    clearBuffer = FrameBuffer (DepthImage n1 100000, ColorImage n1 [0.36, 0.99, 0.99, 1])
    fragStream = Rasterize rasterContext (Transform vert streamSlot)
    cameraView = worldView * worldPosition

    gridPattern :: F V3F -> F Int32 -> F Bool
    gridPattern pos pattern = (pattern == 1) || ((pattern != 0) && isSolid)
      where
        isSolid = solid1 || solid2 || solid3
        rand = 2 * (fract' (sin' (dot' fragCoord [12.9898, 78.233, 0, 0] * 43758.5453)) - 0.5)
        prod1 = dot' pos [2, 2, -2]
        prod2 = dot' pos [2, -2, 2]
        prod3 = dot' pos [-2, 2, 2]
        diff1 = fwidth' prod1
        diff2 = fwidth' prod2
        diff3 = fwidth' prod3
        solid1 = fract' (prod1 + rand * smoothen diff1) < 0.2
        solid2 = fract' (prod2 + rand * smoothen diff2) < 0.2
        solid3 = fract' (prod3 + rand * smoothen diff3) < 0.2
        smoothen x = x * x

    vert :: V InputGeometry -> VertexOut ShadingInput
    vert input = VertexOut (cameraProjection * viewPos) 1 result
      where
        viewPos = cameraView * [input.position, 1]
        result =
            { lightViewPosition = Smooth (lightViewProj * worldPosition * [pos, 1])
            , colour = Flat input.colour
            , localPosition = Smooth input.position
            , eyePosition = Smooth viewPos.xyz
            , eyeNormal = Smooth (normalize' (mat3 cameraView * input.normal)) -- technically this should be the inverse transpose instead of just mat3
            , pattern = Flat input.pattern
            , zBias = Flat input.zBias
            , shininess = Flat input.shininess
            }

    isPartOfGrid :: F ShadingInput -> F Bool
    isPartOfGrid input = gridPattern input.localPosition input.pattern

    frag :: F ShadingInput -> FragmentOut (Depth Float, Color V4F)
    frag input = FragmentOutDepth adjustedDepth litColour
      where
        l = normalize' ((worldView * [lightPosition, 1]).xyz - input.eyePosition)
        e = normalize' (-input.eyePosition)
        n = input.normal
        r = normalize' (reflect' (-l) n)

        lambert = dot l n
        phong = max' (dot r e) 0

        intensity = 0.3 + p_max * (max' 0 lambert * 0.5 + phong ** 5 * 0.3)
        highlight = p_max * (phong ** input.shininess * min' 1.0 (input.shininess * 0.04))

        litColour = [input.colour.xyz * intensity + [1, 1, 1] * highlight, 1]

        fragDepth = fragCoord.z
        adjustedDepth = fragDepth + max' (exp' ((-15) - log' fragDepth)) (fwidth' fragDepth) * input.zBias

        [tx, ty, tz, tw] = input.lightViewPosition
        u = tx / tw * 0.5 + 0.5
        v = ty / tw * 0.5 + 0.5
        [m1, m2, _, _] = texture' shadowSampler [u, v] 0
        variance = max' 0.002 (m2 - m1 * m1)
        d = max' 0 (tz - m1)
        segment x = ceil' (max' 0 (0.5 - abs' (x - 0.5)))
        inShadowTex = 1 - segment u * segment v
        p_max = max' inShadowTex (variance / (variance + d * d))

    shadowSampler = Sampler LinearFilter Clamp shadowMap

    shadowMap :: Texture DIM2 SingleTex (Regular Float) RGBA
    shadowMap = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 moments]

    moments :: FrameBuffer N1 (Float, V4F)
    moments = Accumulate accumContext (Filter isPartOfGrid) moments fragStream clearBuffer
      where
        type MapperInput = (Float, V3F, Int32)
        
        accumContext = {depthOp = (Less, Write), colorOp = (NoBlending, [True, True, True, True])}
        clearBuffer = FrameBuffer (DepthImage n1 100000, ColorImage n1 [0, 0, 1, 1])
        fragStream = Rasterize triangleCtx (Transform vert streamSlot)

        vert :: V InputGeometry -> VertexOut MapperInput
        vert input = VertexOut v4 1 (Smooth mapPosition.z, Smooth input.position, Flat input.pattern)
          where
            mapPosition = lightViewProj * worldPosition * [input.position, 1]

        isPartOfGrid :: F MapperInput -> F Bool
        isPartOfGrid (_, pos, pattern) = gridPattern pos pattern

        moments :: F MapperInput -> FragmentOut (Depth Float, Color V4F)
        moments (depth, _, _) = FragmentOutRastDepth [moment1, moment2, 1, 1]
          where
            dx = dFdx' depth
            dy = dFdy' depth
            moment1 = depth
            moment2 = depth * depth + 0.25 * (dx * dx + dy * dy)
