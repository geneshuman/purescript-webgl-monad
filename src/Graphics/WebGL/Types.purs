module Graphics.WebGL.Types where

import Prelude
import Control.Monad.Except.Trans (ExceptT ())

import Control.Monad.Reader.Trans (ReaderT ())
import Data.ArrayBuffer.Types (Float32Array ())
import Data.Int (toNumber)
import Effect (Effect)

import Graphics.WebGL.Raw.Enums as Enum
import Graphics.WebGL.Raw.Types as Raw

type WebGLT eff a = ReaderT Raw.WebGLContext (ExceptT WebGLError eff) a
type WebGL a = forall eff. WebGLT (Effect) a

data WebGLError
  = ContextLost
  | ErrorCode ErrorCode
  | NullValue String
  | ShaderError String

instance showWebGLError :: Show WebGLError where
  show err = case err of
      ContextLost     -> prefix <> "lost the WebGL context"
      ErrorCode code  -> prefix <> show code
      NullValue fname -> prefix <> "null value in " <> fname <> " (due to an OpenGL error)"
      ShaderError str -> "Shader Error: " <> str
    where
      prefix = "WebGL Error: "

-- re-exported from Raw

type WebGLBuffer            = Raw.WebGLBuffer
type WebGLFramebuffer       = Raw.WebGLFramebuffer
type WebGLTexture           = Raw.WebGLTexture
type WebGLContext           = Raw.WebGLContext
type WebGLContextAttributes = Raw.WebGLContextAttributes
type WebGLProgram           = Raw.WebGLProgram
type WebGLShader            = Raw.WebGLShader
type WebGLUniformLocation   = Raw.WebGLUniformLocation

-- math (this should and will come from a separate library)

data Vec2 = Vec2    Number Number
data Vec3 = Vec3    Number Number Number
data Vec4 = Vec4    Number Number Number Number

data Mat2 = Mat2    Number Number
                    Number Number

data Mat3 = Mat3    Number Number Number
                    Number Number Number
                    Number Number Number

data Mat4 = Mat4    Number Number Number Number
                    Number Number Number Number
                    Number Number Number Number
                    Number Number Number Number

-- attributes and uniforms

newtype Attribute a = Attribute Int
newtype Uniform a   = Uniform WebGLUniformLocation

-- wrapped GLenums

class ToWebGLEnum a where
  toWebglEnum :: a -> Int

class FromWebGLEnum a where
  fromWebglEnum :: Int -> a

data ArrayBufferType
  = ArrayBuffer
  | ElementArrayBuffer

instance toWebglEnumArrayBufferType :: ToWebGLEnum ArrayBufferType where
  toWebglEnum ArrayBuffer        = Enum.arrayBuffer
  toWebglEnum ElementArrayBuffer = Enum.elementArrayBuffer

data BufferType
  = DepthBuffer
  | ColorBuffer
  | StencilBuffer

instance toWebglEnumBufferType :: ToWebGLEnum BufferType where
  toWebglEnum DepthBuffer   = Enum.depthBufferBit
  toWebglEnum ColorBuffer   = Enum.colorBufferBit
  toWebglEnum StencilBuffer = Enum.stencilBufferBit

data BufferUsage
  = DynamicDraw
  | StaticDraw
  | StreamDraw

instance toWebglEnumBufferUsage :: ToWebGLEnum BufferUsage where
  toWebglEnum DynamicDraw = Enum.dynamicDraw
  toWebglEnum StaticDraw  = Enum.staticDraw
  toWebglEnum StreamDraw  = Enum.streamDraw

data DataType
  = Byte
  | UnsignedByte
  | Short
  | UnsignedShort
  | Int
  | UnsignedInt
  | Float

instance toWebglEnumDataType :: ToWebGLEnum DataType where
  toWebglEnum Byte          = Enum.byte
  toWebglEnum UnsignedByte  = Enum.unsignedByte
  toWebglEnum Short         = Enum.short
  toWebglEnum UnsignedShort = Enum.unsignedShort
  toWebglEnum Int           = Enum.int
  toWebglEnum UnsignedInt   = Enum.unsignedInt
  toWebglEnum Float         = Enum.float

data DrawMode
  = Points
  | Lines
  | LineLoop
  | LineStrip
  | Triangles
  | TriangleStrip
  | TriangleFan

instance toWebglEnumDrawMode :: ToWebGLEnum DrawMode where
  toWebglEnum Points        = Enum.points
  toWebglEnum Lines         = Enum.lines
  toWebglEnum LineLoop      = Enum.lineLoop
  toWebglEnum LineStrip     = Enum.lineStrip
  toWebglEnum Triangles     = Enum.triangles
  toWebglEnum TriangleStrip = Enum.triangleStrip
  toWebglEnum TriangleFan   = Enum.triangleFan

data ErrorCode
  = NoError
  | InvalidEnum
  | InvalidValue
  | InvalidOperation
  | OutOfMemory
  | UnknownError

instance fromWebglEnumErrorCode :: FromWebGLEnum ErrorCode where
  fromWebglEnum n
      | n == Enum.noError           = NoError
      | n == Enum.invalidEnum       = InvalidEnum
      | n == Enum.invalidValue      = InvalidValue
      | n == Enum.invalidOperation  = InvalidOperation
      | otherwise                   = UnknownError

instance showErrorCode :: Show ErrorCode where
  show NoError          = "no error"
  show InvalidEnum      = "invalid enum"
  show InvalidValue     = "invalid value"
  show InvalidOperation = "invalid operation"
  show OutOfMemory      = "out of memory"
  show _                = "unknown error"

data ProgramParam
  = DeleteStatus
  | LinkStatus
  | ValidateStatus
  | AttachedShaders
  | ActiveAttrs
  | ActiveUniforms

instance toWebglEnumProgramParam :: ToWebGLEnum ProgramParam where
  toWebglEnum DeleteStatus    = Enum.deleteStatus
  toWebglEnum LinkStatus      = Enum.linkStatus
  toWebglEnum ValidateStatus  = Enum.validateStatus
  toWebglEnum AttachedShaders = Enum.attachedShaders
  toWebglEnum ActiveAttrs     = Enum.activeAttributes
  toWebglEnum ActiveUniforms  = Enum.activeUniforms

data ShaderParam
  = ShaderTypeSP
  | DeleteStatusSP
  | CompileStatusSP

instance toWebglEnumShaderParam :: ToWebGLEnum ShaderParam where
  toWebglEnum ShaderTypeSP     = Enum.shaderType
  toWebglEnum DeleteStatusSP   = Enum.deleteStatus
  toWebglEnum CompileStatusSP  = Enum.compileStatus


data ShaderType
  = FragmentShader
  | VertexShader

instance toWebglEnumShader :: ToWebGLEnum ShaderType where
  toWebglEnum FragmentShader = Enum.fragmentShader
  toWebglEnum VertexShader   = Enum.vertexShader

-- params for polymorphic raw functions

data BufferData
  = DataSize Int
  | DataSource Float32Array
