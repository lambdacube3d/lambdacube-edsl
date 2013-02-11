module LC_G_APIType where

import Data.Data
import Data.Int
import Data.Word

data ProvokingVertex
    = FirstVertex
    | LastVertex
    deriving (Eq,Ord,Bounded,Enum,Show)

data FrontFace
    = CCW
    | CW
    deriving (Eq,Ord,Show)

data CullMode
    = CullNone
    | CullFront FrontFace
    | CullBack  FrontFace
    deriving (Eq,Ord,Show)

type DepthFunction = ComparisonFunction

data ComparisonFunction
    = Never
    | Less
    | Equal
    | Lequal
    | Greater
    | Notequal
    | Gequal
    | Always
    deriving (Eq, Ord, Show)

data StencilOperation
    = OpZero
    | OpKeep
    | OpReplace
    | OpIncr
    | OpIncrWrap
    | OpDecr
    | OpDecrWrap
    | OpInvert
    deriving (Eq, Ord, Show)

data BlendEquation
    = FuncAdd
    | FuncSubtract
    | FuncReverseSubtract
    | Min
    | Max
    deriving (Eq, Ord, Show)

data BlendingFactor
    = Zero
    | One
    | SrcColor
    | OneMinusSrcColor
    | DstColor
    | OneMinusDstColor
    | SrcAlpha
    | OneMinusSrcAlpha
    | DstAlpha
    | OneMinusDstAlpha
    | ConstantColor
    | OneMinusConstantColor
    | ConstantAlpha
    | OneMinusConstantAlpha
    | SrcAlphaSaturate
    deriving (Eq, Ord, Show)

data LogicOperation
    = Clear
    | And
    | AndReverse
    | Copy
    | AndInverted
    | Noop
    | Xor
    | Or
    | Nor
    | Equiv
    | Invert
    | OrReverse
    | CopyInverted
    | OrInverted
    | Nand
    | Set
    deriving (Eq, Ord, Show)

data StencilOps
    = StencilOps
    { frontStencilOp    :: StencilOperation -- ^ Used for front faced triangles and other primitives.
    , backStencilOp     :: StencilOperation -- ^ Used for back faced triangles.
    } deriving (Eq,Ord,Show)

-- sampler and texture specification
data Filter
    = PointFilter
    | LinearFilter
    deriving (Show,Eq,Ord)

data EdgeMode
    = Wrap
    | Mirror
    | Clamp
    deriving (Show,Eq,Ord)
