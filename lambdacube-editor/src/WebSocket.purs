module WebSocket where

import Control.Monad.Eff
import Data.Either

foreign import data WS :: !
foreign import data Socket :: *

type WSAction eff a = Eff (ws :: WS | eff) a

type WebSocketHandler eff =
  { onOpen    :: Socket -> WSAction eff Unit
  , onClose   :: WSAction eff Unit
  , onMessage :: Socket -> String -> WSAction eff Unit
  , onError   :: Socket -> String -> WSAction eff Unit
  }

foreign import webSocketImpl """
  function webSocketImpl(uri) {
   return function(h) {
    return function(ok) {
     return function(err) {
      return function() {
        try {
          var ws = new WebSocket(uri);
          ws.onerror = function(event) {
            h.onError(ws)(event.data)();
          };
  
          ws.onopen = function() {
            h.onOpen(ws)();
          };
  
          ws.onclose = function() {
            h.onClose();
          };
  
          ws.onmessage = function(event) {
            h.onMessage(ws)(event.data)();
          };
          return ok(ws);
        } catch (e) {
          console.log("exception");
          console.log(e);
          console.log(e.type);
          return err(e.type);
        }
      };
     };
    };
   };
  }
  """ :: forall eff . String -> WebSocketHandler eff -> (Socket -> Either String Socket) -> (String -> Either String Socket) -> WSAction eff (Either String Socket)

webSocket :: forall eff . String -> WebSocketHandler eff -> WSAction eff (Either String Socket)
webSocket a b = webSocketImpl a b Right Left

foreign import send """
  function send (socket) {
    return function (data) {
      return function () {
        socket.send(data);
      };
    };
  }
  """ :: forall eff . Socket -> String -> WSAction eff Unit
