module Main where

import Control.Monad.Eff.Alert
import Control.Monad.Eff
import Debug.Trace

import qualified Graphics.WebGL as GL

import Backend
import IR
import Data.Maybe
import Data.StrMap
import Data.Tuple

samplePipeline =
  { textures : []
  , samplers : []
  , targets :
        [   { renderTargets : [ {semantic:Color , ref:Just (Framebuffer Color) } ] }
        ]
  , programs : []
  , slots : []
  , commands :
      [ SetRenderTarget 0
      , ClearRenderTarget [ {semantic: Color , value: VV4F (V4 1.0 0.0 0.4 1.0) } ]
      ]
  }

gfx03Pipeline =
  { textures : []
  , samplers : []
  , targets :
      [   { renderTargets :
              [ { semantic: Depth , ref: Just (Framebuffer Depth) }
              , { semantic: Color , ref: Just (Framebuffer Color) }
              ]
          }
      ]
  , programs:
      [   { programUniforms : fromList [ Tuple "MVP2" M44F ]
          , programStreams : fromList [ Tuple "v" {name: "position" , ty: V3F } ]
          , programInTextures : fromList []
          , programOutput : [ {name: "f0" , ty: V4F } ]
          , vertexShader :
              "#version 330 core\nuniform mat4 MVP2 ;\nin vec3 v ;\nvoid main() {\ngl_Position = ( MVP2 ) * ( vec4( v ,1.0) );\ngl_PointSize = 1.0;\n}\n"
          , geometryShader : Nothing
          , fragmentShader :
              "#version 330 core\nout vec4 f0 ;\nvoid main() {\nf0 = vec4 ( 0.0,0.4,0.0,1.0 );\n}\n"
          }
      ,   { programUniforms : fromList [ Tuple "MVP2" M44F ]
          , programStreams : fromList [ Tuple "v" {name: "position" , ty: V3F } ]
          , programInTextures : fromList []
          , programOutput : [ {name: "f0" , ty: V4F } ]
          , vertexShader :
              "#version 330 core\nuniform mat4 MVP2 ;\nin vec3 v ;\nsmooth out vec4 v0 ;\nvoid main() {\nv0 = vec4( v ,1.0);\ngl_Position = ( MVP2 ) * ( vec4( v ,1.0) );\ngl_PointSize = 1.0;\n}\n"
          , geometryShader : Nothing
          , fragmentShader :
              "#version 330 core\nsmooth in vec4 v0 ;\nout vec4 f0 ;\nvoid main() {\nf0 = ( v0 ) + ( vec4 ( 1.0,1.4,1.0,0.6 ) );\n}\n"
          }
      ,   { programUniforms : fromList [ Tuple "MVP" M44F ]
          , programStreams : fromList [ Tuple "v" {name: "position4" , ty: V4F } ]
          , programInTextures : fromList []
          , programOutput : [ {name: "f0" , ty: V4F } ]
          , vertexShader :
              "#version 330 core\nuniform mat4 MVP ;\nin vec4 v ;\nflat out vec4 v0 ;\nvoid main() {\nv0 = v;\ngl_Position = ( MVP ) * ( v );\ngl_PointSize = 1.0;\n}\n"
          , geometryShader : Nothing
          , fragmentShader :
              "#version 330 core\nflat in vec4 v0 ;\nout vec4 f0 ;\nvoid main() {\nf0 = ( v0 ) * ( vec4 ( 1.0,1.4,1.0,0.6 ) );\n}\n"
          }
      ]
  , slots:
      [   { slotName : "stream"
          , slotUniforms : fromList [ Tuple "MVP2" M44F ]
          , slotStreams : fromList [ Tuple "position" V3F ]
          , slotPrimitive : Triangles
          , slotPrograms : [ 0 , 1 ]
          }
      ,   { slotName : "stream4"
          , slotUniforms : fromList [ Tuple "MVP" M44F ]
          , slotStreams : fromList [ Tuple "position4" V4F ]
          , slotPrimitive : Triangles
          , slotPrograms : [ 2 ]
          }
      ]
  , commands:
      [ SetRenderTarget 0
      , ClearRenderTarget
          [ {semantic: Depth , value: VFloat 1000.0 }
          , {semantic: Color , value: VV4F (V4 0.5 0.0 0.4 1.0) }
          ]
      , SetRasterContext
          (TriangleCtx CullNone PolygonFill NoOffset LastVertex)
      , SetAccumulationContext
            { accViewportName : Nothing
            , accOperations :
                [ DepthOp Less false
                , ColorOp
                    (Blend
                       {colorEq: FuncAdd , alphaEq: FuncAdd }
                       { colorF: {src: SrcAlpha , dst: OneMinusSrcAlpha }
                       , alphaF: {src: SrcAlpha , dst: OneMinusSrcAlpha }
                       }
                       (V4 1.0 1.0 1.0 1.0))
                    (VV4B (V4 true true true true))
                ]
            }
      , SetProgram 2
      , RenderSlot 1
      , SetRasterContext
          (TriangleCtx CullNone PolygonFill NoOffset FirstVertex)
      , SetAccumulationContext
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
      , SetAccumulationContext
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

main :: Eff (trace :: Trace, alert :: Alert) Unit
main = GL.runWebGL "glcanvas" (\s -> alert s)
  \ context -> do
    trace "WebGL ready"
    ppl <- allocPipeline samplePipeline
    trace "Pipeline allocated"
    renderPipeline ppl
    trace "WebGL completed"
    disposePipeline ppl
    trace "Pipeline disposed"

