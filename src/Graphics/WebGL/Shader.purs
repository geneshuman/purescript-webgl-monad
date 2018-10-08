module Graphics.WebGL.Shader
( addShaderToProgram
, compileShadersIntoProgram
, getAttrBindings
, getUniformBindings
, linkProgram
) where

import Prelude
import Graphics.WebGL.Types (Attribute(..), ProgramParam(..), ShaderParam(..), ShaderType(..), Uniform(..), Vec2(..), Vec3(..), Vec4(..), WebGL, WebGLContext, WebGLError(..), WebGLProgram, WebGLShader, WebGLUniformLocation)
import Graphics.WebGL.Methods as GL
import Graphics.WebGL.Raw as GLR
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (ask)
import Data.Foldable (foldl)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.WebGL.Raw.Util (toMaybe)

foreign import data Object :: Type -> Type

class SetVertAttr a where
  setVertAttr :: Attribute a -> a -> WebGL Unit

instance setVertAttrNumber :: SetVertAttr Number where
  setVertAttr attr x = GL.vertexAttrib1f attr x

instance setVertAttrVec2 :: SetVertAttr Vec2 where
  setVertAttr attr (Vec2 x y) = GL.vertexAttrib2f attr x y

instance setVertAttrVec3 :: SetVertAttr Vec3 where
  setVertAttr attr (Vec3 x y z) = GL.vertexAttrib3f attr x y z

instance setVertAttrVec4 :: SetVertAttr Vec4 where
  setVertAttr attr (Vec4 x y z w) = GL.vertexAttrib4f attr x y z w

class SetUniform a where
  setUniform :: Uniform a -> a -> WebGL Unit

instance setUniformNumber :: SetUniform Number where
  setUniform unif x = GL.uniform1f unif x

instance setUniformVec2 :: SetUniform Vec2 where
  setUniform unif (Vec2 x y) = GL.uniform2f unif x y

instance setUniformVec3 :: SetUniform Vec3 where
  setUniform unif (Vec3 x y z) = GL.uniform3f unif x y z

instance setUniformVec4 :: SetUniform Vec4 where
  setUniform unif (Vec4 x y z w) = GL.uniform4f unif x y z w

-- constants

shaderLinkError :: WebGLError
shaderLinkError = ShaderError "could not link shaders prog"

-- public functions

addShaderToProgram :: WebGLProgram -> ShaderType -> String -> WebGL Unit
addShaderToProgram prog stype src = do
    shader <- GL.createShader stype
    GL.shaderSource shader src
    GL.compileShader shader
    GL.attachShader prog shader

checkShader :: WebGLShader -> WebGL Unit
checkShader shader = do
    status <- GL.getShaderParameter shader CompileStatusSP
    when (not status) do
      ctx <- ask
      res <- liftEffect $ GLR.getShaderSource ctx shader
      err <- GL.getShaderInfoLog shader
      case res of
        Just src -> throwError $ ShaderError ("\n\n--------------SHADER-------------:" <> (annotateSource src) <> "\n\n--------------ERRORS--------------\n" <> err)
        Nothing -> throwError $ ShaderError "no source?"

annotateSource :: String -> String
annotateSource src =
  snd $ foldl handle (Tuple 1 "") (split (Pattern "\n") src)
  where
    handle (Tuple n dt) l = Tuple (n + 1) (dt <> "\n" <> (pad (show n) 3) <> " - " <> l)

pad :: String -> Int -> String
pad s w | (length s < w) = pad (" " <> s) w
pad s _ = s

compileShadersIntoProgram :: String -> String -> WebGL WebGLProgram
compileShadersIntoProgram vertSrc fragSrc = do
    prog <- GL.createProgram
    addShaderToProgram prog VertexShader vertSrc
    addShaderToProgram prog FragmentShader fragSrc

    pure prog

-- checks for errors & shit
linkProgram :: WebGLProgram -> WebGL Unit
linkProgram prog = do
  GL.linkProgram prog
  ctx <- ask
  shaders <- liftEffect $ GLR.getAttachedShaders ctx prog
  _ <- traverse checkShader shaders

  isLinked <- GL.getProgramParameter prog LinkStatus
  when (not isLinked) (throwError shaderLinkError)
  pure unit

getAttrBindings :: forall bindings. WebGLProgram -> WebGL (Record bindings)
getAttrBindings prog = do
    ctx <- ask
    result <- liftEffect $ getAttrBindings_ ctx prog
    case result of
      Just val -> pure val
      Nothing -> throwError $ NullValue "getAttrBindings"

getUniformBindings :: forall bindings. WebGLProgram -> WebGL (Record bindings)
getUniformBindings prog = do
    ctx <- ask
    result <- liftEffect $ getUniformBindings_ ctx prog
    case result of
      Just val -> pure val
      Nothing -> throwError $ NullValue "getUniformBindings"

-- foreigns

foreign import getAttrBindingsImpl :: forall bindings a. Fn3 WebGLContext WebGLProgram (Int -> Attribute a) (Effect(Record bindings))

getAttrBindings_ :: forall bindings. WebGLContext -> WebGLProgram -> Effect(Maybe (Record bindings))
getAttrBindings_ ctx prog = runFn3 getAttrBindingsImpl ctx prog Attribute >>= toMaybe >>> pure

foreign import getUniformBindingsImpl :: forall bindings a. Fn3 WebGLContext WebGLProgram (WebGLUniformLocation -> Uniform a) (Effect(Record bindings))

getUniformBindings_ :: forall bindings. WebGLContext -> WebGLProgram -> Effect(Maybe (Record bindings))
getUniformBindings_ ctx prog = runFn3 getUniformBindingsImpl ctx prog Uniform >>= toMaybe >>> pure
