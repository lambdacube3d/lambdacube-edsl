
simple' :: FrameBuffer N1 (Float,V4F)
simple' = FrameBuffer (DepthImage n1 1000,ColorImage n1 (one'::V4F))

simple :: VertexStream Triangle (V3F,V3F) -> FrameBuffer N1 (Float,V4F)
simple objs = Accumulate fragCtx (Filter filter) frag rast clear
  where
    rastCtx :: RasterContext Triangle
    rastCtx = TriangleCtx (CullFront CW) PolygonFill NoOffset LastVertex
    
    fragCtx :: FragmentOperation (Depth Float, Color (V4 Float))
    fragCtx = (DepthOp Less True, ColorOp NoBlending (one' :: V4B))
    
    clear :: FrameBuffer N1 (Float,V4F)
    clear   = FrameBuffer (DepthImage n1 1000,ColorImage n1 (zero'::V4F))
    
    rast :: FragmentStream N1 V3F
    rast    = Rasterize rastCtx prims

    prims :: PrimitiveStream Triangle N1 V V3F
    prims   = Transform vert objs

    worldViewProj :: V M44F
    worldViewProj = uniformM44F

    vert :: V (V3F,V3F) -> VertexOut V3F
    vert (p,n) = VertexOut v4 1 (Flat (drop4 v4))
      where
        v4 :: V V4F
        v4 = worldViewProj *. snoc p 1

    frag :: F V3F -> FragmentOut (Depth Float, Color V4F)
    frag a = FragmentOutRastDepth $ (snoc a 1)

    filter :: F a -> F Bool
    filter a = (primitiveID % 2) == 0

lcnet :: Image N1 V4F
lcnet = PrjFrameBuffer tix0 $ simple $ Fetch "streamSlot" Triangle (IV3F "position", IV3F "normal")

lcnet' :: Image N1 V4F
lcnet' = PrjFrameBuffer "outFB" tix0 $ simple'
