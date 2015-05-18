module Main (main,run) where

import Debug.Trace

import Control.Monad.Eff

import Ace
import Ace.Types
import qualified Ace.Editor as Editor
import qualified Ace.EditSession as Session
import qualified Ace.Range as Range

import qualified Graphics.WebGL as GL
import qualified Control.Monad.JQuery as J

import WebSocket
import DefaultText
import Data.Either
import Sample

import Data.Maybe
import Data.StrMap
import Data.Tuple

import Backend
import IR
import Mesh
import Type
import Input

import PipelineJsonDecode
import qualified Data.Argonaut as A

import qualified Data.Matrix as M
import qualified Data.Matrix4 as M

main = return unit
{-
  done - websocket setup
  done - compile button
  done - lc compiler in snap server
  done - json serializer for Pipeline
  done - embed lambdacube-webgl
    done - setup pipeline input
    done - compile/replace pipeline
  show error messages
  trace result
-}

{-
  control-b - compile/build
  control-n - new
-}
foreign import addCommand """
  function addCommand(editor) {
    return function(cmdName) {
      return function(winKey) {
        return function(macKey) {
          return function(cmd) {
            return function() {
              editor.commands.addCommand({
                name: cmdName,
                bindKey: {win: winKey,  mac: macKey},
                exec: function(editor) {
                  cmd(editor)();
                }
              });
            };
          };
        };
      };
    };
  }
""" :: forall eff . Editor -> String -> String -> String -> (Editor -> Eff (ace :: EAce | eff) Unit) -> Eff (ace :: EAce | eff) Unit

foreign import tokenTooltip """
  function tokenTooltip(editor) {
    return function() {
        editor.tokenTooltip = new myTokenTooltip(editor);
    };
  }
""" :: forall eff . Editor -> Eff (ace :: EAce | eff) Unit

run = GL.runWebGL "glcanvas" (\s -> trace s) $ \context -> do
  -- setup pipeline input
  let inputSchema = 
        { slots : fromList [ Tuple "stream"  {primitive: Triangles, attributes: fromList [Tuple "position"  TV3F, Tuple "normal" TV3F, Tuple "UVTex" TV2F]}
                           , Tuple "stream4" {primitive: Triangles, attributes: fromList [Tuple "position4" TV4F, Tuple "vertexUV" TV2F]}
                           ]
        , uniforms : fromList [Tuple "MVP" M44F, Tuple "MVP2" M44F]
        }
  pplInput <- mkWebGLPipelineInput inputSchema
  let mvp = V4 (V4 0.4692207 (-0.28573585) (-0.9593549) (-0.9574381)) (V4 0.0 2.395976 (-0.122928835) (-0.12268323)) (V4 (-1.719497) (-7.797232e-2) (-0.2617912) (-0.26126814)) (V4 0.0 0.0 3.8834958 4.0755367)
  uniformM44F "MVP" pplInput.uniformSetter mvp

  gpuCube <- compileMesh myCube

  addMesh pplInput "stream4" gpuCube []

  -- setup ace editor
  editor <- Ace.edit "editor" ace
  session <- Editor.getSession editor
  Editor.setTheme "ace/theme/terminal" editor
  Session.setMode "ace/mode/haskell" session
  Session.setValue defaultSrc session
  range <- Range.create 8 0 9 0
  Session.addMarker range "lc_error" "line" false session
  tokenTooltip editor

  b <- J.body
  ui <- J.find "ui" b
  btnCompile <- J.create "<button>"
  J.setText "Compile" btnCompile
  btnCompile `J.append` b

  let compile s = do
        src <- Session.getValue session
        send s src

      render ir = do
        trace "WebGL ready"
        ppl <- allocPipeline ir -- gfx03Pipeline -- samplePipeline
        trace "Pipeline allocated"

        setPipelineInput ppl (Just pplInput)
        sortSlotObjects pplInput
        trace "Setup pipeline input"

        renderPipeline ppl
        trace "WebGL completed"
        disposePipeline ppl
        trace "Pipeline disposed"

  socket <- webSocket "ws://localhost:8000/console/bash" $
    { onOpen    : \s -> do
        trace "socket is ready"
        compile s
    , onClose   : trace "socket is closed"
    , onMessage : \s m -> do
        trace m
        case A.jsonParser m >>= A.decodeJson of
          Left e -> trace $ "decode error: " ++ e
          Right (MyLeft e) -> trace $ "compile error: " ++ e
          Right (MyRight p) -> render p
    , onError   : \s m -> trace m
    }
  case socket of
    Left m -> trace $ "error: " ++ m
    Right ws -> do
      flip (J.on "click") btnCompile $ \_ _ -> do
        compile ws
        trace "clicked compile"
      addCommand editor "Compile" "Ctrl-B" "Command-B" (\_ -> compile ws)
      return unit
