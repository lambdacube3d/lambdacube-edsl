simpleRendering :: FrameBuffer 1 (Float, Float[4])
simpleRendering = accumulate accumContext True frag fragStream clearBuffer
  where
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
        transform :: (a@V -> b@V*) -> Triangle a@I -> b@V

    -- cameraView :: Float[4,4]@C
    cameraView = worldView * worldPosition

    -- cameraLightDirection :: Float[3]@C
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

blurredRendering :: FrameBuffer 1 (Float, Float[4])
blurredRendering = separableBlur gaussWeights7 gaussWeights7 imageToBlur
  where
    -- imageToBlur :: Image 1 V4F
    imageToBlur = prjFrameBuffer 0 simpleRendering

separableBlur :: Array (Float, Float) -> Array (Float, Float) -> Image 1 Float[4] -> FrameBuffer 1 (Float, Float[4])
separableBlur weightsH weightsV img = blur True weightsH (prjFrameBuffer 0 (blur False weightsV img))
  where
    -- size :: Number a => a
    size = 512

    blur :: Bool -> Array (Float, Float) -> Image 1 Float[4] -> FrameBuffer 1 (Float, Float[4])
    blur isHorizontal weights img = accumulate accumContext passAll frag fragStream clearBuffer
      where
        type BlurBuffer = (Depth Float, Color Float[4])

        -- accumContext :: AccumulationContext BlurBuffer
        accumContext = { depthOp = (Always, Keep)
                       , colorOp = (NoBlending, [True, True, True, True])
                       }

        -- clearBuffer :: FrameBuffer 1 BlurBuffer
        clearBuffer = FrameBuffer (DepthImage 1 1000, ColorImage 1 [0, 0, 0, 1])

        -- fragStream :: FragmentStream layerCount V2F
        fragStream = rasterize defaultTriangleContext (transform vert (fetchArray TriangleList quadVertices))

        frag :: Float[2]@F ~> BlurBuffer@F*
        frag uv = fragmentOutRasterDepth (sum <<texture' smp (uv + offset d) 0 * w | (d, w) <- weights>>)  -- note: array comprehension is fully static
          where
	        -- offset :: Number a => a@p ~> (Vector 2 a)@p
            offset x = if isHorizontal then [x / size, 0] else [0, x / size]
            smp = Sampler LinearFilter Clamp tex
            tex = Texture (Texture2D (Float RGBA) n1) [size, size] NoMip [img]

        vert :: Float[2]@V ~> Float[2]@V*
        vert uv = vertexOut [uv, 1, 1] 1 (noPerspective (uv * 0.5 + 0.5))

quadVertices :: Array Float[2]
quadVertices = <<[-1, 1], [-1, -1], [1, -1], [1, -1], [1, 1], [-1, 1]>>

gaussWeights7 :: Array (Float, Float)
gaussWeights7 = <<(-3, 0.015625), (-2, 0.09375), (-1, 0.234375), (0, 0.3125), (1, 0.234375), (2, 0.09375), (3, 0.015625)>>

-- Language elements:

if_then_else_ :: Bool -> a -> a -> a  {-or-}  Bool@p ~> a@p ~> a@p ~> a@p  -- determined by the type of Bool used

-- Types of main pipeline building blocks:

-- note: the type prim depends on the value of the first argument (type PrimitiveInterpretation)
fetchArray :: Primitive prim => PrimitiveInterpretation -> Array a -> VertexStream prim a
fetchIndexedArray :: Primitive prim => PrimitiveInterpretation -> Array a -> Array Int -> VertexStream prim a
transform :: Primitive primIn, Primitive primOut => (a@V ~> b@V*) -> VertexStream primIn a -> PrimitiveStream primOut Vertex 1 b
reassemble :: Primitive primIn, Primitive primOut => GeometryShader primIn primOut layerCount a b -> PrimitiveStream primIn Vertex 1 a -> PrimitiveStream primOut Geometry layerCount b
rasterize :: Primitive prim => RasterContext prim -> PrimitiveStream prim (Vertex | Geometry) layerCount a -> FragmentStream layerCount a
accumulate :: AccumulationContext b -> (a@F ~> Bool@F) -> (a@F ~> b@F*) -> FragmentStream layerCount a -> FrameBuffer layerCount b -> FrameBuffer layerCount b

Primitive p1, Primitive p2 => GeometryShader p1 p2 n a b ~ (n, p2, Int, PrimitiveVertices p1 a ~> (i, Int)@G, i@G ~> (i, j, Int)@G, j@G ~> (j, b)@G*)

-- Multi-pass:

-- note: it is somewhat confusing that index 0 means the last element of the tuple, i.e. it is right to left
prjFrameBuffer :: Projection a b -> FrameBuffer layerCount a -> Image layerCount b

-- Projection a b is essentially a subset of (a -> b), and it will
-- most likely be superseded by deconstruction through pattern
-- matching (on a tuple that models the framebuffer)

-- Other functions:

sum :: Number a => Array a -> a

-- we know from the concrete Number instance (which includes phase
-- information as well) what kind of addition and zero to use, and
-- expand it statically
