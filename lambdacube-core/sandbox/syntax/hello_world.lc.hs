blurredImage :: FrameBuffer 1 V4F
blurredImage = separableBlur gaussWeights7 gaussWeights7 imageToBlur
  where
    imageToBlur = prjFrameBuffer 0 simpleRendering
    simpleRendering = accumulate ... -- whatever we want to blur

separableBlur :: Array (Float, Float) -> Array (Float, Float) -> Image 1 V4F -> FrameBuffer 1 (Float, V4F)
separableBlur weightsH weightsV img = blur True weightsH (prjFrameBuffer 0 (blur False weightsV img))
  where
    size = 512

    blur :: Bool -> Image 1 V4F -> FrameBuffer 1 (Float, V4F)
    blur isHorizontal weights img = accumulate accumContext passAll frag fragStream clearBuffer
      where
        record QuadGeometry = { uv :: V2F }
        record SamplingCoordinates = { uv :: V2F }
        -- This could be defined as constant geometry right here instead of a stream slot to populate from outside
        stream postSlot :: Triangle QuadGeometry

        accumContext = {depthOp = (Always, Keep), colorOp = (NoBlending, [True, True, True, True])}
        clearBuffer = FrameBuffer (DepthImage n1 1000, ColorImage n1 [1 0 0 1])
        fragStream = rasterize defaultTriangleContext (transform vert postSlot)

        frag :: SamplingCoordinates@F ~> (Depth Float, Color V4F)@F*
        frag input = fragmentOutRasterDepth (sum <<texture' smp (input.uv + offset d) 0 * w | (d, w) <- weights>>)
          where
            offset x = if isHorizontal then [x / size, 0] else [0, x / size]
            smp = Sampler LinearFilter Clamp tex
            tex = Texture (Texture2D (Float RGBA) n1) [size, size] NoMip [img]

        vert :: QuadGeometry@V -> SamplingCoordinates@V*
        vert input = vertexOut [input.uv, 1, 1] 1 { uv = noPerspective (input.uv * 0.5 + 0.5) }

gaussWeights7 :: Array (Float, Float)
gaussWeights7 = <<(-3.0, 0.015625), (-2.0, 0.09375), (-1.0, 0.234375), (0.0, 0.3125), (1.0, 0.234375), (2.0, 0.09375), (3.0, 0.015625)>>
