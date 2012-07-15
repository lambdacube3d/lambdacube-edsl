simpleRendering :: FrameBuffer 1 ColorWithDepth
simpleRendering = accumulate accumContext True frag fragStream clearBuffer
  where
    accumulate :: TODO

    type ColorWithDepth = BufferDepth Float Float[4]

    const worldPosition :: Float[4,4]
          worldView :: Float[4,4]
          cameraProjection :: Float[4,4]
          lightDirection :: Float[3]

    record InputGeometry = { position :: Float[3]
                           , normal :: Float[3]
                           }

    stream inputStream :: Triangle InputGeometry

    {-
     -- builtin type families
     record AccumulationContext (BufferDepth d c) =
      { depthOp :: (Comparison d, Combine d)
      , colorOp :: (Blending c, Mask c)
      }
     record AccumulationContext (BufferFlat c) =
      { colorOp :: (Blending c, Mask c) }

     type Mask a[n] = Bool[n]
    -}
    accumContext :: AccumulationContext ColorWithDepth
    accumContext = { depthOp = (Less, Write)
                   , colorOp = (NoBlending, [True, True, True, True])
                   }

    {-
      data RasterContext Triangle = TriangleContext{ .. } | ..
    -}
    rasterContext :: RasterContext Triangle
    rasterContext = TriangleContext { cullMode = None
                                    , polygonMode = Fill
                                    , polygonOffset = None
                                    , provokingVertex = Last
                                    }

    clearBuffer :: FrameBuffer 1 ColorWithDepth
    {-
      data FrameBuffer n (BufferDepth d c) = FrameBuffer (DepthImage n d, ColorImage n c)

      data ColorImage n c = ColorImage n c -- not a dependent type, 'n' is simply an int literal that is part of the constructor's structured name
      data DepthImage n d = ColorImage n d
    -}
    clearBuffer = FrameBuffer (DepthImage 1 10000, ColorImage 1 [0, 0, 0, 1])

    -- fragStream :: FragmentStream layerCount Float[4]
    fragStream = Rasterize rasterContext (Transform vert inputStream)
      where
        -- ??
        transform :: (a@V -> b@V*) -> TODO
        -- ??
        rasterize :: (Prim prim) => RasterContext prim -> TODO

    cameraView :: Float[4,4]@C
    cameraView = worldView * worldPosition

    cameraLightDirection :: Float[3]@C
    cameraLightDirection = (cameraView * [lightDirection, 0]).xyz

    frag :: Float[3]@F -> ColorWithDepth@F*
    frag normal = fragmentOutRasterDepth lightIntensity
      where
        lightIntensity :: Float
        lightIntensity = max 0 (dot
                                (normalize cameraLightDirection)
                                (normalize normal))

        max :: (Ord a) => a@p -> a@p -> a@p
        dot :: (Vector a) => a@p -> a@p -> a@p
        normalize :: (Vector a) => a@p -> a@p

        -- XXX but lightIntensity has type Float and our return type is BufferDepth Float Float[4]...
        fragmentOutRasterDepth :: a@F -> BufferDepth Float a@F*

    vert :: InputGeometry@V -> Float[3]@V*
    vert input = vertexOut position 1 (smooth normal)
      where
        position :: Float[4]@V
        position = cameraProjection * cameraView * [input.position, 1]

        normal :: Float[3]@V
        normal = (worldPosition * [input.normal, 0]).xyz

        1 :: (Num a) => a@p
        (*) :: (Linear a) => a@p -> a@p -> a@p
        vertexOut :: Float[4]@V -> Float@V -> Interp a@V -> a@V*
        smooth :: (Continuous a) => a@p -> Interp a@p
