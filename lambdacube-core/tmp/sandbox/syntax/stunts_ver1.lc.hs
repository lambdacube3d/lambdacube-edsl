{-
  removed:
    GP
    Exp
    Const
    @
    tup/untup
    pack/unpack
    $

  changed:
    snoc, trim -> swizzling
    dot product to dot'
    not equals to !=
    vector construction to []
    one' :: V4B to explicit vector of True values
    trimM4 to mat3 (any better idea for matrix swizzling?)
    pow' to **
-}
stuntsGFX :: FrameBuffer N1 (Float, V4F)
stuntsGFX = Accumulate fragCtx (Filter fragPassed) frag rast clear
  where
    fragCtx = (DepthOp Less True, ColorOp NoBlending [True, True, True, True])
    rastCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex
    clear   = FrameBuffer (DepthImage n1 100000, ColorImage n1 [0.36, 0.99, 0.99, 1])
    rast    = Rasterize rastCtx (Transform vert (Fetch "streamSlot" Triangle input))
    input   = (IV3F "position", IV3F "normal", IV4F "colour", IInt "pattern", IFloat "zBias", IFloat "shininess")
    const   worldPosition   :: M44F
            worldView       :: M44F
            projection      :: M44F
            lightPosition   :: V3F
            lightViewProj   :: M44F
    camMat = worldView .*. worldPosition

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

    vert :: V (V3F, V3F, V4F, Int32, Float, Float) -> VertexOut (V4F, V4F, V3F, V3F, V3F, Int32, Float, Float)
    vert (pos, n, colour, pattern, zBias, shiny) =
        VertexOut projPos 1 (Smooth lightViewPos, Flat colour, Smooth pos, Smooth eyePos, Smooth normal, Flat pattern, Flat zBias, Flat shiny)
      where
        viewPos = camMat *. [pos, 1]
        projPos = projection *. viewPos
        normal = normalize' (mat3 camMat *. n)
        eyePos = viewPos.xyz
        lightViewPos = lightViewProj *. worldPosition *. [pos, 1]

    fragPassed :: F (V4F, V4F, V3F, V3F, V3F, Int32, Float, Float) -> F Bool
    fragPassed (_, _, pos, _, _, pattern, _, _) = gridPattern pos pattern

    frag :: F (V4F, V4F, V3F, V3F, V3F, Int32, Float, Float) -> FragmentOut (Depth Float, Color V4F)
    frag (lightViewPos, colour, pos, eyePos, normal, pattern, zBias, shiny) = FragmentOutDepth adjustedDepth litColour
      where
        l = normalize' ((worldView *. [lightPosition, 1]).xyz - eyePos)
        e = normalize' (-eyePos)
        n = normal
        r = normalize' (reflect' (-l) n)

        lambert = dot l n
        phong = max' (dot r e) 0

        intensity = 0.3 + p_max * (max' 0 lambert * 0.5 + phong ** 5 * 0.3)
        highlight = p_max * (phong ** shiny * min' 1.0 (shiny * 0.04))

        litColour = [colour.xyz * intensity + [1, 1, 1] * highlight, 1]

        fragDepth = fragCoord.z
        adjustedDepth = fragDepth + max' (exp' ((-15) - log' fragDepth)) (fwidth' fragDepth) * zBias

        [tx, ty, tz, tw] = lightViewPos
        u = tx / tw * 0.5 + 0.5
        v = ty / tw * 0.5 + 0.5
        [m1, m2, _, _] = texture' sampler [u, v] 0
        variance = max' 0.002 (m2 - m1 * m1)
        d = max' 0 (tz - m1)
        segment x = ceil' (max' 0 (0.5 - abs' (x - 0.5)))
        inShadowTex = 1 - segment u * segment v
        p_max = max' inShadowTex (variance / (variance + d * d))

    sampler = Sampler LinearFilter Clamp shadowMap

    shadowMap :: Texture DIM2 SingleTex (Regular Float) RGBA
    shadowMap = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 moments]

    moments :: FrameBuffer N1 (Float, V4F)
    moments = Accumulate fragCtx (Filter fragPassed) storeDepth rast clear
      where
        fragCtx = DepthOp Less True:.ColorOp NoBlending [True, True, True, True]
        clear   = FrameBuffer (DepthImage n1 100000, ColorImage n1 [0, 0, 1, 1])
        rast    = Rasterize triangleCtx prims
        prims   = Transform vert input
        input   = Fetch "streamSlot" Triangle (IV3F "position", IInt "pattern")
        const   worldPosition :: M44F
                lightViewProj :: M44F

        vert :: V (V3F, Int32) -> VertexOut (Float, V3F, Int32)
        vert (pos, pattern) = VertexOut v4 1 (Smooth depth, Smooth pos, Flat pattern)
          where
            v4 = lightViewProj *. worldPosition *. [pos, 1]
            depth = v4.z

        fragPassed :: F (Float, V3F, Int32) -> F Bool
        fragPassed (_, pos, pattern) = gridPattern pos pattern

        storeDepth :: F (Float, V3F, Int32) -> FragmentOut (Depth Float, Color V4F)
        storeDepth (depth, _, _) = FragmentOutRastDepth [moment1, moment2, 1, 1]
          where
            dx = dFdx' depth
            dy = dFdy' depth
            moment1 = depth
            moment2 = depth * depth + 0.25 * (dx * dx + dy * dy)
