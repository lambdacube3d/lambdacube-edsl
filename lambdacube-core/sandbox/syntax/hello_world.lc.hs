simpleRendering :: FrameBuffer 1 (Float, V4F)
simpleRendering = accumulate accumContext True frag fragStream clearBuffer
  where
    type ColorWithDepth = (Depth Float, Color V4F)

    const worldPosition :: M44F
          worldView :: M44F
          cameraProjection :: M44F
          lightDirection :: V3F

    record InputGeometry = { position :: V3F, normal :: V3F }
    
    stream inputStream :: Triangle InputGeometry
    
    -- accumContext :: AccumulationContext ColorWithDepth
    accumContext = {depthOp = (Less, Write), colorOp = (NoBlending, [True, True, True, True])}

    -- rasterContext :: RasterContext Triangle
    rasterContext = TriangleContext {cullMode = None, polygonMode = Fill, polygonOffset = None, provokingVertex = Last}

    -- clearBuffer :: FrameBuffer 1 ColorWithDepth
    clearBuffer = FrameBuffer (DepthImage 1 10000, ColorImage 1 [0, 0, 0, 1])

    -- fragStream :: FragmentStream layerCount V4F
    fragStream = Rasterize rasterContext (Transform vert inputStream)

    -- cameraView :: M44F@C
    cameraView = worldView * worldPosition

    -- cameraLightDirection :: V3F@C
    cameraLightDirection = (cameraView * [lightDirection, 0]).xyz

    frag :: V3F@F ~> ColorWithDepth@F*
    frag normal = fragmentOutRasterDepth lightIntensity
      where
        lightIntensity = max 0 (dot (normalize cameraLightDirection) (normalize normal))

    vert :: InputGeometry@V ~> V3F@V*
    vert input = vertexOut position 1 (smooth normal)
      where
        position = cameraProjection * cameraView * [input.position, 1]
        normal = (worldPosition * [input.normal, 0]).xyz

blurredRendering :: FrameBuffer 1 (Float, V4F)
blurredRendering = separableBlur gaussWeights7 gaussWeights7 imageToBlur
  where
    -- imageToBlur :: Image 1 V4F
    imageToBlur = prjFrameBuffer 0 simpleRendering

separableBlur :: Array (Float, Float) -> Array (Float, Float) -> Image 1 V4F -> FrameBuffer 1 (Float, V4F)
separableBlur weightsH weightsV img = blur True weightsH (prjFrameBuffer 0 (blur False weightsV img))
  where
    -- size :: Number a => a
    size = 512

    blur :: Bool -> Image 1 V4F -> FrameBuffer 1 (Float, V4F)
    blur isHorizontal weights img = accumulate accumContext passAll frag fragStream clearBuffer
      where
        type BlurBuffer = (Depth Float, Color V4F)

        -- accumContext :: AccumulationContext BlurBuffer
        accumContext = {depthOp = (Always, Keep), colorOp = (NoBlending, [True, True, True, True])}

        -- clearBuffer :: FrameBuffer 1 BlurBuffer
        clearBuffer = FrameBuffer (DepthImage 1 1000, ColorImage 1 [0, 0, 0, 1])

        -- fragStream :: FragmentStream layerCount V2F
        fragStream = rasterize defaultTriangleContext (transform vert (fetchArray TriangleList quadVertices))

        frag :: V2F@F ~> BlurBuffer@F*
        frag uv = fragmentOutRasterDepth (sum <<texture' smp (uv + offset d) 0 * w | (d, w) <- weights>>)  -- note: array comprehension is fully static
          where
	        -- offset :: Number a => a@p ~> (Vector 2 a)@p
            offset x = if isHorizontal then [x / size, 0] else [0, x / size]
            smp = Sampler LinearFilter Clamp tex
            tex = Texture (Texture2D (Float RGBA) n1) [size, size] NoMip [img]

        vert :: V2F@V ~> V2F@V*
        vert uv = vertexOut [uv, 1, 1] 1 (noPerspective (uv * 0.5 + 0.5))

quadVertices :: Array V2F
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
