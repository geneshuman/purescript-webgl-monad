module Graphics.WebGL.Context
( defaultWebglContextAttrs
, getWebglContext
, getWebglContextWithAttrs
, canvasElement
, drawingBufferHeight
, drawingBufferWidth
) where

import Prelude
import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (ask)
import Data.Function (Fn4 (..), runFn4)
import Data.Maybe (Maybe (..))
import Graphics.Canvas (Canvas (), CanvasElement ())
import Graphics.WebGL.Unsafe (unsafeCoerce)

import Graphics.WebGL.Types

defaultWebglContextAttrs :: WebGLContextAttributes
defaultWebglContextAttrs =
  { alpha:                            true
  , depth:                            true
  , stencil:                          false
  , antialias:                        true
  , premultipliedAlpha:               true
  , preserveDrawingBuffer:            false
  , preferLowPowerToHighPerformance:  false
  , failIfMajorPerformanceCaveat:     false
  }

getWebglContextWithAttrs :: forall eff. CanvasElement -> WebGLContextAttributes -> Eff (canvas :: Canvas | eff) (Maybe WebGLContext)
getWebglContextWithAttrs canvas attrs = runFn4 getWebglContextWithAttrsImpl canvas attrs Just Nothing

getWebglContext :: forall eff. CanvasElement -> Eff (canvas :: Canvas | eff) (Maybe WebGLContext)
getWebglContext canvas = getWebglContextWithAttrs canvas defaultWebglContextAttrs

-- context properties

canvasElement :: WebGL CanvasElement
canvasElement = _.canvas <$> contextProperties

drawingBufferHeight :: WebGL Int
drawingBufferHeight = _.drawingBufferHeight <$> contextProperties

drawingBufferWidth :: WebGL Int
drawingBufferWidth = _.drawingBufferWidth <$> contextProperties

-- private functions

type ContextProperties =
  { canvas              :: CanvasElement
  , drawingBufferWidth  :: Int
  , drawingBufferHeight :: Int
  }

contextProperties :: WebGL ContextProperties
contextProperties = ask >>= (unsafeCoerce :: WebGLContext -> ContextProperties) >>> return

-- foreigns

foreign import getWebglContextWithAttrsImpl :: forall eff maybe. Fn4 CanvasElement WebGLContextAttributes (WebGLContext -> maybe) maybe (Eff (canvas :: Canvas | eff) (Maybe WebGLContext))
