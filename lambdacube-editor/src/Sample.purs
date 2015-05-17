module Sample where

import Data.Maybe
import Data.StrMap
import Data.Tuple

import Backend
import IR
import Mesh
import Type
import Input

--  Our vertices. Tree consecutive floats give a 3D vertex; Three consecutive vertices give a triangle.
--  A cube has 6 faces with 2 triangles each, so this makes 6*2=12 triangles, and 12*3 vertices
g_vertex_buffer_data =
    [ V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0  (-1.0) (-1.0) 1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0    1.0    1.0  1.0
    , V4   1.0  (-1.0)   1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0  (-1.0)   1.0  1.0
    , V4   1.0  (-1.0) (-1.0) 1.0
    , V4   1.0    1.0    1.0  1.0
    , V4 (-1.0) (-1.0)   1.0  1.0
    , V4   1.0  (-1.0)   1.0  1.0
    , V4   1.0    1.0    1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0) (-1.0)   1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4 (-1.0) (-1.0)   1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4   1.0    1.0    1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0    1.0    1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    ]

--  Two UV coordinatesfor each vertex. They were created with Blender.
g_uv_buffer_data =
    [ V2 0.0 0.0
    , V2 0.0 1.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 1.0 0.0
    , V2 0.0 0.0
    , V2 1.0 0.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 0.0 1.0
    , V2 1.0 0.0
    , V2 0.0 1.0
    , V2 1.0 1.0
    , V2 1.0 0.0
    , V2 0.0 0.0
    , V2 0.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 0.0 1.0
    , V2 0.0 0.0
    , V2 1.0 0.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 0.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 0.0 1.0
    , V2 0.0 0.0
    , V2 0.0 1.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 1.0 0.0
    ]

myCube :: Mesh
myCube =
    { attributes: fromList
        [ Tuple "position4" (A_V4F g_vertex_buffer_data)
        , Tuple "vertexUV"  (A_V2F g_uv_buffer_data)
        ]
    , primitive: P_Triangles
    , gpuData: Nothing
    }

samplePipeline = Pipeline
  { textures : []
  , samplers : []
  , targets :
        [ RenderTarget { renderTargets : [ TargetItem {semantic:Color , ref:Just (Framebuffer Color) } ] }
        ]
  , programs : []
  , slots : []
  , commands :
      [ SetRenderTarget 0
      , ClearRenderTarget [ ClearImage {semantic: Color , value: VV4F (V4 0.0 0.0 0.4 1.0) } ]
      ]
  }

gfx03Pipeline = Pipeline
  { textures : []
  , samplers : []
  , targets :
      [ RenderTarget
          { renderTargets :
              [ TargetItem { semantic: Depth , ref: Just (Framebuffer Depth) }
              , TargetItem { semantic: Color , ref: Just (Framebuffer Color) }
              ]
          }
      ]
  , programs:
      [ Program
          { programUniforms : fromList [ Tuple "MVP2" M44F ]
          , programStreams : fromList [ Tuple "v" (Parameter {name: "position" , ty: V3F }) ]
          , programInTextures : fromList []
          , programOutput : [ Parameter {name: "gl_FragColor" , ty: V4F } ]
          , vertexShader :
              "#version 100\nprecision highp float;\nprecision highp int;\nuniform mat4 MVP2 ;\nattribute vec3 v ;\nvoid main() {\ngl_Position = ( MVP2 ) * (vec4( v ,1.0) );\ngl_PointSize = 1.0;\n}\n"
          , geometryShader : Nothing
          , fragmentShader :
              "#version 100\nprecision highp float;\nprecision highp int;\nvoid main() {\ngl_FragColor = vec4 ( 0.0,0.4,0.0,1.0 );\n}\n"
          }
      , Program
          { programUniforms : fromList [ Tuple "MVP2" M44F ]
          , programStreams : fromList [ Tuple "v" (Parameter {name: "position" , ty: V3F }) ]
          , programInTextures : fromList []
          , programOutput : [ Parameter {name: "gl_FragColor" , ty: V4F } ]
          , vertexShader :
              "#version 100\nprecision highp float;\nprecision highp int;\nuniform mat4 MVP2 ;\nattribute vec3 v ;\nvarying vec4 v0 ;\nvoid main() {\nv0 = vec4( v ,1.0);\ngl_Position = ( MVP2 ) * ( vec4( v ,1.0) );\ngl_PointSize = 1.0;\n}\n"
          , geometryShader : Nothing
          , fragmentShader :
              "#version 100\nprecision highp float;\nprecision highp int;\nvarying vec4 v0 ;\nvoid main() {\ngl_FragColor = ( v0 ) + ( vec4 ( 1.0,1.4,1.0,0.6 ) );\n}\n"
          }
      , Program
          { programUniforms : fromList [ Tuple "MVP" M44F ]
          , programStreams : fromList [ Tuple "v" (Parameter {name: "position4" , ty: V4F }) ]
          , programInTextures : fromList []
          , programOutput : [ Parameter {name: "gl_FragColor" , ty: V4F } ]
          , vertexShader :
              "#version 100\nprecision highp float;\nprecision highp int;\nuniform mat4 MVP ;\nattribute vec4 v ;\nvarying vec4 v0 ;\nvoid main() {\nv0 = v;\ngl_Position = ( MVP ) * ( v );\ngl_PointSize = 1.0;\n}\n"
          , geometryShader : Nothing
          , fragmentShader :
              "#version 100\nprecision highp float;\nprecision highp int;\nvarying vec4 v0 ;\nvoid main() {\ngl_FragColor = ( v0 ) * ( vec4 ( 1.0,1.4,1.0,0.6 ) );\n}\n"
          }
      ]
  , slots:
      [ Slot
          { slotName : "stream"
          , slotUniforms : fromList [ Tuple "MVP2" M44F ]
          , slotStreams : fromList [ Tuple "position" V3F ]
          , slotPrimitive : Triangles
          , slotPrograms : [ 0 , 1 ]
          }
      , Slot
        { slotName : "stream4"
          , slotUniforms : fromList [ Tuple "MVP" M44F ]
          , slotStreams : fromList [ Tuple "position4" V4F ]
          , slotPrimitive : Triangles
          , slotPrograms : [ 2 ]
          }
      ]
  , commands:
      [ SetRenderTarget 0
      , ClearRenderTarget
          [ ClearImage {semantic: Depth , value: VFloat 1000.0 }
          , ClearImage {semantic: Color , value: VV4F (V4 0.5 0.0 0.4 1.0) }
          ]
      , SetRasterContext
          (TriangleCtx CullNone PolygonFill NoOffset LastVertex)
      , SetAccumulationContext $ AccumulationContext
            { accViewportName : Nothing
            , accOperations :
                [ DepthOp Less false
                , ColorOp
                    (Blend
                       {colorEq: FuncAdd , alphaEq: FuncAdd }
                       { colorF: BlendingFactorPair {src: SrcAlpha , dst: OneMinusSrcAlpha }
                       , alphaF: BlendingFactorPair {src: SrcAlpha , dst: OneMinusSrcAlpha }
                       }
                       (V4 1.0 1.0 1.0 1.0))
                    (VV4B (V4 true true true true))
                ]
            }
      , SetProgram 2
      , RenderSlot 1
      , SetRasterContext
          (TriangleCtx CullNone PolygonFill NoOffset FirstVertex)
      , SetAccumulationContext $ AccumulationContext
            { accViewportName : Nothing
            , accOperations :
                [ DepthOp Less false
                , ColorOp NoBlending (VV4B (V4 true true false false))
                ]
            }
      , SetProgram 1
      , RenderSlot 0
      , SetRasterContext
          (TriangleCtx CullNone (PolygonLine 20.0) NoOffset FirstVertex)
      , SetAccumulationContext $ AccumulationContext
            { accViewportName : Nothing
            , accOperations :
                [ DepthOp Always false
                , ColorOp NoBlending (VV4B (V4 true true false false))
                ]
            }
      , SetProgram 0
      , RenderSlot 0
      ]
  }
