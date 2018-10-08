module Graphics.WebGL.Methods where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Error.Class (throwError)
import Data.ArrayBuffer.Types (Float32Array ())
import Data.Maybe (Maybe (..))

import Graphics.WebGL.Raw as Raw

import Graphics.WebGL.Types

attachShader :: WebGLProgram -> WebGLShader -> WebGL Unit
attachShader prog shader = do
    ctx <- ask
    liftEffect $ Raw.attachShader ctx prog shader

bindBuffer :: ArrayBufferType -> WebGLBuffer -> WebGL Unit
bindBuffer btype buffer = do
    ctx <- ask
    liftEffect $ Raw.bindBuffer ctx (toWebglEnum btype) buffer

bufferData :: ArrayBufferType -> BufferData -> BufferUsage -> WebGL Unit
bufferData btype datatype usage = do
    ctx <- ask
    liftEffect $ case datatype of
      (DataSource ns) -> Raw.bufferData ctx (toWebglEnum btype) ns (toWebglEnum usage)
      (DataSize n)    -> Raw.bufferData_ ctx (toWebglEnum btype) n (toWebglEnum usage)

clear :: BufferType -> WebGL Unit
clear buffer = do
    ctx <- ask
    liftEffect $ Raw.clear ctx $ toWebglEnum buffer

clearColor :: Number -> Number -> Number -> Number -> WebGL Unit
clearColor r g b a = do
    ctx <- ask
    liftEffect $ Raw.clearColor ctx r g b a

compileShader :: WebGLShader -> WebGL Unit
compileShader shader = do
    ctx <- ask
    liftEffect $ Raw.compileShader ctx shader

createBuffer :: WebGL WebGLBuffer
createBuffer = do
    result <- ask >>= Raw.createBuffer >>> liftEffect
    case result of
      Just buffer -> pure buffer
      Nothing -> throwError $ NullValue "createBuffer"

createTexture :: WebGL WebGLTexture
createTexture = do
    result <- ask >>= Raw.createTexture >>> liftEffect
    case result of
      Just texture -> pure texture
      Nothing -> throwError $ NullValue "createTexture"

createFramebuffer :: WebGL WebGLFramebuffer
createFramebuffer = do
    result <- ask >>= Raw.createFramebuffer >>> liftEffect
    case result of
      Just buffer -> pure buffer
      Nothing -> throwError $ NullValue "createFramebuffer"

createProgram :: WebGL WebGLProgram
createProgram = do
    ctx <- ask
    result <- liftEffect $ Raw.createProgram ctx
    case result of
      Just prog -> pure prog
      Nothing -> throwError $ NullValue "createProgram"

createShader :: ShaderType -> WebGL WebGLShader
createShader stype = do
    ctx <- ask
    result <- liftEffect $ Raw.createShader ctx $ toWebglEnum stype
    case result of
      Just shader -> pure shader
      Nothing -> throwError $ NullValue "createShader"

drawArrays :: DrawMode -> Int -> Int -> WebGL Unit
drawArrays mode first count = do
    ctx <- ask
    liftEffect $ Raw.drawArrays ctx (toWebglEnum mode) first count

enableVertexAttribArray :: forall a. Attribute a -> WebGL Unit
enableVertexAttribArray (Attribute attr) = do
    ctx <- ask
    liftEffect $ Raw.enableVertexAttribArray ctx attr

getError :: WebGL Int
getError = ask >>= Raw.getError >>> liftEffect

getProgramParameter :: forall a. WebGLProgram -> ProgramParam -> WebGL a
getProgramParameter prog param = do
    ctx <- ask
    result <- liftEffect $ Raw.getProgramParameter ctx prog $ toWebglEnum param
    case result of
      Just val -> pure val
      Nothing -> throwError $ NullValue "getProgramParameter"

getShaderParameter :: forall a. WebGLShader -> ShaderParam -> WebGL a
getShaderParameter prog param = do
    ctx <- ask
    result <- liftEffect $ Raw.getShaderParameter ctx prog $ toWebglEnum param
    case result of
      Just val -> pure val
      Nothing -> throwError $ NullValue "getShaderParameter"

getShaderInfoLog :: forall a. WebGLShader -> WebGL String
getShaderInfoLog shader = do
    ctx <- ask
    result <- liftEffect $ Raw.getShaderInfoLog ctx shader
    case result of
      Just val -> pure val
      Nothing -> throwError $ NullValue "getShaderInfoLog"

isContextLost :: WebGL Boolean
isContextLost = ask >>= Raw.isContextLost >>> liftEffect

linkProgram :: WebGLProgram -> WebGL Unit
linkProgram prog = do
    ctx <- ask
    liftEffect $ Raw.linkProgram ctx prog

shaderSource :: WebGLShader -> String -> WebGL Unit
shaderSource shader src = do
    ctx <- ask
    liftEffect $ Raw.shaderSource ctx shader src

uniform1f :: forall u. Uniform u -> Number -> WebGL Unit
uniform1f (Uniform attr) x = do
    ctx <- ask
    liftEffect $ Raw.uniform1f ctx attr x

uniform1fv :: forall u. Uniform u -> Float32Array -> WebGL Unit
uniform1fv (Uniform attr) xs = do
    ctx <- ask
    liftEffect $ Raw.uniform1fv_ ctx attr xs

uniform2f :: forall u. Uniform u -> Number -> Number -> WebGL Unit
uniform2f (Uniform attr) x y = do
    ctx <- ask
    liftEffect $ Raw.uniform2f ctx attr x y

uniform2fv :: forall u. Uniform u -> Float32Array -> WebGL Unit
uniform2fv (Uniform attr) xs = do
    ctx <- ask
    liftEffect $ Raw.uniform2fv_ ctx attr xs

uniform3f :: forall u. Uniform u -> Number -> Number -> Number -> WebGL Unit
uniform3f (Uniform attr) x y z = do
    ctx <- ask
    liftEffect $ Raw.uniform3f ctx attr x y z

uniform3fv :: forall u. Uniform u -> Float32Array -> WebGL Unit
uniform3fv (Uniform attr) xs = do
    ctx <- ask
    liftEffect $ Raw.uniform3fv_ ctx attr xs

uniform4f :: forall u. Uniform u -> Number -> Number -> Number -> Number -> WebGL Unit
uniform4f (Uniform attr) x y z w = do
    ctx <- ask
    liftEffect $ Raw.uniform4f ctx attr x y z w

uniform4fv :: forall u. Uniform u -> Float32Array -> WebGL Unit
uniform4fv (Uniform attr) xs = do
    ctx <- ask
    liftEffect $ Raw.uniform4fv_ ctx attr xs

useProgram :: WebGLProgram -> WebGL Unit
useProgram prog = do
    ctx <- ask
    liftEffect $ Raw.useProgram ctx prog

vertexAttrib1f :: forall a. Attribute a -> Number -> WebGL Unit
vertexAttrib1f (Attribute a) x = do
    ctx <- ask
    liftEffect $ Raw.vertexAttrib1f ctx a x

vertexAttrib1fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
vertexAttrib1fv (Attribute a) xs = do
    ctx <- ask
    liftEffect $ Raw.vertexAttrib1fv_ ctx a xs

vertexAttrib2f :: forall a. Attribute a -> Number -> Number -> WebGL Unit
vertexAttrib2f (Attribute a) x y = do
    ctx <- ask
    liftEffect $ Raw.vertexAttrib2f ctx a x y

vertexAttrib2fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
vertexAttrib2fv (Attribute a) xs = do
    ctx <- ask
    liftEffect $ Raw.vertexAttrib2fv_ ctx a xs

vertexAttrib3f :: forall a. Attribute a -> Number -> Number -> Number -> WebGL Unit
vertexAttrib3f (Attribute a) x y z = do
    ctx <- ask
    liftEffect $ Raw.vertexAttrib3f ctx a x y z

vertexAttrib3fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
vertexAttrib3fv (Attribute a) xs = do
    ctx <- ask
    liftEffect $ Raw.vertexAttrib3fv_ ctx a xs

vertexAttrib4f :: forall a. Attribute a -> Number -> Number -> Number -> Number -> WebGL Unit
vertexAttrib4f (Attribute a) x y z w = do
    ctx <- ask
    liftEffect $ Raw.vertexAttrib4f ctx a x y z w

vertexAttrib4fv :: forall a. Attribute a -> Float32Array -> WebGL Unit
vertexAttrib4fv (Attribute a) xs = do
    ctx <- ask
    liftEffect $ Raw.vertexAttrib4fv_ ctx a xs

vertexAttribPointer :: forall a. Attribute a -> Int -> DataType -> Boolean -> Int -> Int -> WebGL Unit
vertexAttribPointer (Attribute attr) size dtype isNormalized stride offset = do
    ctx <- ask
    liftEffect $ Raw.vertexAttribPointer ctx attr size (toWebglEnum dtype) isNormalized stride offset
