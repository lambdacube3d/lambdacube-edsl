import LCDSL
import LCDSLContext
import LCDSLLinAlg
import LCDSLTexture

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

{-
    Test cases:
        - unit input
            - point
            - line
            - triangle
            
            - with GS
            - without GS
            - with Filter
            - without Filter
-}

depthImage :: Image N1 Float
depthImage = NewImage n1 0

-- point, input: (), no GS, no Filter
renderPoints = accumulate accCtx PassAll fragmentShader fragmentStream depthImage
  where
    accCtx :: AccumulationContext (FragmentOperation (Depth Float))
    accCtx = AccumulationContext Nothing (DepthOp Never True)

    v0 = V4 0 0 0 0
    fragmentShader _ = fragmentOutRastDepth ()

    fragmentStream  = rasterize PointCtx primitiveStream

    primitiveStream = transform vertexShader vertexStream 

    vertexShader _  = vertexOut v0 0 ()

    vertexStream :: VertexStream Point ()
    vertexStream = input

-- point, input: V4F, GS, no Filter
renderPointsToTriangles = accumulate accCtx PassAll fragmentShader fragmentStream depthImageL2
  where
    depthImageL2    :: Image N2 Float
    depthImageL2    = NewImage n2 0

    accCtx :: AccumulationContext (FragmentOperation (Depth Float))
    accCtx = AccumulationContext Nothing (DepthOp Never True)

    v0 = V4 0 0 0 0
    fragmentShader _ = fragmentOutRastDepth ()

    fragmentStream  = rasterize defaultTriangleCtx geometryStream

    primitiveStream = transform vertexShader vertexStream 

    geometryStream  = reassemble geometryShader primitiveStream

    geometryShader  = GeometryShader n2 Triangle 100 primNum vertNum verts
      where
        primNum v  = (v,0)
        vertNum v  = (v,v,0)
        verts v    = geometryOut v 0 0 0 v (linear v)

    vertexShader v  = vertexOut v 1 (smooth v)

    vertexStream :: VertexStream Point V4F
    vertexStream = input


pipeline1 :: Output
pipeline1 = screenOut renderPoints

pipeline2 :: Output
pipeline2 = imageOut renderPointsToTriangles

