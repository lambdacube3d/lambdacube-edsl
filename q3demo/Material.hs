module Material where

import Data.Vect
import qualified Data.ByteString.Char8 as SB

identityLight :: Float
identityLight = 1

data Entity
    = Entity
    { eAmbientLight     :: Vec4
    , eDirectedLight    :: Vec4
    , eLightDir         :: Vec3
    , eShaderRGBA       :: Vec4
    }

data WaveType
    = WT_Sin
    | WT_Triangle
    | WT_Square
    | WT_Sawtooth
    | WT_InverseSawtooth
    | WT_Noise

data Wave = Wave !WaveType !Float !Float !Float !Float

data Deform
    = D_AutoSprite
    | D_AutoSprite2
    | D_Bulge !Float !Float !Float
    | D_Move !Vec3 !Wave
    | D_Normal !Float !Float
    | D_ProjectionShadow
    | D_Text0
    | D_Text1
    | D_Text2
    | D_Text3
    | D_Text4
    | D_Text5
    | D_Text6
    | D_Text7
    | D_Wave !Float !Wave

data CullType
    = CT_FrontSided
    | CT_BackSided
    | CT_TwoSided

data CommonAttrs
    = CommonAttrs
    { caSkyParms        :: !() -- TODO
    , caFogParms        :: !() -- TODO
    , caPortal          :: !Bool
    , caSort            :: !Int -- default: 3 or 6 depends on blend function
    , caEntityMergable  :: !Bool
    , caFogOnly         :: !Bool
    , caCull            :: !CullType -- default: front
    , caDeformVertexes  :: ![Deform]
    , caNoMipMaps       :: !Bool
    , caPolygonOffset   :: !Bool
    , caStages          :: ![StageAttrs]
    }

defaultCommonAttrs :: CommonAttrs
defaultCommonAttrs = CommonAttrs
    { caSkyParms        = ()
    , caFogParms        = ()
    , caPortal          = False
    , caSort            = 3
    , caEntityMergable  = False
    , caFogOnly         = False
    , caCull            = CT_FrontSided
    , caDeformVertexes  = []
    , caNoMipMaps       = False
    , caPolygonOffset   = False
    , caStages          = []
    }

data RGBGen
    = RGB_Wave !Wave
    | RGB_Const !Float !Float !Float
    | RGB_Identity
    | RGB_IdentityLighting
    | RGB_Entity
    | RGB_OneMinusEntity
    | RGB_ExactVertex
    | RGB_Vertex
    | RGB_LightingDiffuse
    | RGB_OneMinusVertex

data AlphaGen
    = A_Wave !Wave
    | A_Const !Float
    | A_Portal
    | A_Identity
    | A_Entity
    | A_OneMinusEntity
    | A_Vertex
    | A_LightingSpecular
    | A_OneMinusVertex

data TCGen
    = TG_Base
    | TG_Lightmap
    | TG_Environment -- TODO, check: RB_CalcEnvironmentTexCoords
    | TG_Vector !Vec3 !Vec3

data TCMod
    = TM_EntityTranslate
    | TM_Rotate !Float
    | TM_Scroll !Float !Float
    | TM_Scale !Float !Float
    | TM_Stretch !Wave
    | TM_Transform !Float !Float !Float !Float !Float !Float
    | TM_Turb !Float !Float !Float !Float

data StageTexture
    = ST_Map        !SB.ByteString
    | ST_ClampMap   !SB.ByteString
    | ST_AnimMap    !Float ![SB.ByteString]
    | ST_Lightmap
    | ST_WhiteImage
    deriving (Show, Eq, Ord)

data AlphaFunction
    = A_Gt0
    | A_Lt128
    | A_Ge128
    deriving (Show, Eq, Ord)

data DepthFunction
    = D_Equal
    | D_Lequal

data Blending
    = B_DstAlpha
    | B_DstColor
    | B_One
    | B_OneMinusDstAlpha
    | B_OneMinusDstColor
    | B_OneMinusSrcAlpha
    | B_OneMinusSrcColor
    | B_SrcAlpha
    | B_SrcAlphaSaturate
    | B_SrcColor
    | B_Zero

data StageAttrs
    = StageAttrs
    { saBlend       :: !(Maybe (Blending,Blending))
    , saRGBGen      :: !RGBGen
    , saAlphaGen    :: !AlphaGen
    , saTCGen       :: !TCGen
    , saTCMod       :: ![TCMod]
    , saTexture     :: !StageTexture
    , saDepthWrite  :: !Bool
    , saDepthFunc   :: !DepthFunction
    , saAlphaFunc   :: !(Maybe AlphaFunction)
    }

defaultStageAttrs :: StageAttrs
defaultStageAttrs = StageAttrs
    { saBlend       = Nothing
    , saRGBGen      = RGB_IdentityLighting
    , saAlphaGen    = A_Identity
    , saTCGen       = TG_Base
    , saTCMod       = []
    , saTexture     = ST_WhiteImage
    , saDepthWrite  = True--False
    , saDepthFunc   = D_Lequal
    , saAlphaFunc   = Nothing
    }

fixAttribOrder :: CommonAttrs -> CommonAttrs
fixAttribOrder ca = ca
    { caDeformVertexes = reverse $ caDeformVertexes ca
    , caStages = reverse $ map fixStage $ caStages ca
    }
  where
    fixStage sa = sa {saTCMod = reverse $ saTCMod sa}
