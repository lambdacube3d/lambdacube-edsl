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
    deriving Show

data Wave = Wave !WaveType !Float !Float !Float !Float deriving Show

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
    deriving Show

data CullType
    = CT_FrontSided
    | CT_BackSided
    | CT_TwoSided
    deriving Show

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
    deriving Show

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
    deriving Show

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
    deriving Show

data TCGen
    = TG_Base
    | TG_Lightmap
    | TG_Environment -- TODO, check: RB_CalcEnvironmentTexCoords
    | TG_Vector !Vec3 !Vec3
    | TG_Undefined  -- FIXME: HACK!!
    deriving Show

data TCMod
    = TM_EntityTranslate
    | TM_Rotate !Float
    | TM_Scroll !Float !Float
    | TM_Scale !Float !Float
    | TM_Stretch !Wave
    | TM_Transform !Float !Float !Float !Float !Float !Float
    | TM_Turb !Float !Float !Float !Float
    deriving Show

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
    deriving Show

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
    deriving (Show, Eq)

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
    deriving Show

defaultStageAttrs :: StageAttrs
defaultStageAttrs = StageAttrs
    { saBlend       = Nothing
    , saRGBGen      = RGB_IdentityLighting
    , saAlphaGen    = A_Identity
    , saTCGen       = TG_Undefined
    , saTCMod       = []
    , saTexture     = ST_WhiteImage
    , saDepthWrite  = True--False
    , saDepthFunc   = D_Lequal
    , saAlphaFunc   = Nothing
    }

{-
typedef struct {
	char	*name;
	int		clearSolid, surfaceFlags, contents;
} infoParm_t;

infoParm_t	infoParms[] = {
	// server relevant contents
	{"water",		1,	0,	CONTENTS_WATER },
	{"slime",		1,	0,	CONTENTS_SLIME },		// mildly damaging
	{"lava",		1,	0,	CONTENTS_LAVA },		// very damaging
	{"playerclip",	1,	0,	CONTENTS_PLAYERCLIP },
	{"monsterclip",	1,	0,	CONTENTS_MONSTERCLIP },
	{"nodrop",		1,	0,	CONTENTS_NODROP },		// don't drop items or leave bodies (death fog, lava, etc)
	{"nonsolid",	1,	SURF_NONSOLID,	0},						// clears the solid flag

	// utility relevant attributes
	{"origin",		1,	0,	CONTENTS_ORIGIN },		// center of rotating brushes
	{"trans",		0,	0,	CONTENTS_TRANSLUCENT },	// don't eat contained surfaces
	{"detail",		0,	0,	CONTENTS_DETAIL },		// don't include in structural bsp
	{"structural",	0,	0,	CONTENTS_STRUCTURAL },	// force into structural bsp even if trnas
	{"areaportal",	1,	0,	CONTENTS_AREAPORTAL },	// divides areas
	{"clusterportal", 1,0,  CONTENTS_CLUSTERPORTAL },	// for bots
	{"donotenter",  1,  0,  CONTENTS_DONOTENTER },		// for bots

	{"fog",			1,	0,	CONTENTS_FOG},			// carves surfaces entering
	{"sky",			0,	SURF_SKY,		0 },		// emit light from an environment map
	{"lightfilter",	0,	SURF_LIGHTFILTER, 0 },		// filter light going through it
	{"alphashadow",	0,	SURF_ALPHASHADOW, 0 },		// test light on a per-pixel basis
	{"hint",		0,	SURF_HINT,		0 },		// use as a primary splitter

	// server attributes
	{"slick",		0,	SURF_SLICK,		0 },
	{"noimpact",	0,	SURF_NOIMPACT,	0 },		// don't make impact explosions or marks
	{"nomarks",		0,	SURF_NOMARKS,	0 },		// don't make impact marks, but still explode
	{"ladder",		0,	SURF_LADDER,	0 },
	{"nodamage",	0,	SURF_NODAMAGE,	0 },
	{"metalsteps",	0,	SURF_METALSTEPS,0 },
	{"flesh",		0,	SURF_FLESH,		0 },
	{"nosteps",		0,	SURF_NOSTEPS,	0 },

	// drawsurf attributes
	{"nodraw",		0,	SURF_NODRAW,	0 },	// don't generate a drawsurface (or a lightmap)
	{"pointlight",	0,	SURF_POINTLIGHT, 0 },	// sample lighting at vertexes
	{"nolightmap",	0,	SURF_NOLIGHTMAP,0 },	// don't generate a lightmap
	{"nodlight",	0,	SURF_NODLIGHT, 0 },		// don't ever add dynamic lights
	{"dust",		0,	SURF_DUST, 0}			// leave a dust trail when walking on this surface
};

#define	CONTENTS_SOLID			1		// an eye is never valid in a solid
#define	CONTENTS_LAVA			8
#define	CONTENTS_SLIME			16
#define	CONTENTS_WATER			32
#define	CONTENTS_FOG			64

#define	CONTENTS_AREAPORTAL		0x8000

#define	CONTENTS_PLAYERCLIP		0x10000
#define	CONTENTS_MONSTERCLIP	0x20000
//bot specific contents types
#define	CONTENTS_TELEPORTER		0x40000
#define	CONTENTS_JUMPPAD		0x80000
#define CONTENTS_CLUSTERPORTAL	0x100000
#define CONTENTS_DONOTENTER		0x200000

#define	CONTENTS_ORIGIN			0x1000000	// removed before bsping an entity

#define	CONTENTS_BODY			0x2000000	// should never be on a brush, only in game
#define	CONTENTS_CORPSE			0x4000000
#define	CONTENTS_DETAIL			0x8000000	// brushes not used for the bsp
#define	CONTENTS_STRUCTURAL		0x10000000	// brushes used for the bsp
#define	CONTENTS_TRANSLUCENT	0x20000000	// don't consume surface fragments inside
#define	CONTENTS_TRIGGER		0x40000000
#define	CONTENTS_NODROP			0x80000000	// don't leave bodies or items (death fog, lava)

#define	SURF_NODAMAGE			0x1		// never give falling damage
#define	SURF_SLICK				0x2		// effects game physics
#define	SURF_SKY				0x4		// lighting from environment map
#define	SURF_LADDER				0x8
#define	SURF_NOIMPACT			0x10	// don't make missile explosions
#define	SURF_NOMARKS			0x20	// don't leave missile marks
#define	SURF_FLESH				0x40	// make flesh sounds and effects
#define	SURF_NODRAW				0x80	// don't generate a drawsurface at all
#define	SURF_HINT				0x100	// make a primary bsp splitter
#define	SURF_SKIP				0x200	// completely ignore, allowing non-closed brushes
#define	SURF_NOLIGHTMAP			0x400	// surface doesn't need a lightmap
#define	SURF_POINTLIGHT			0x800	// generate lighting info at vertexes
#define	SURF_METALSTEPS			0x1000	// clanking footsteps
#define	SURF_NOSTEPS			0x2000	// no footstep sounds
#define	SURF_NONSOLID			0x4000	// don't collide against curves with this set
#define SURF_LIGHTFILTER		0x8000	// act as a light filter during q3map -light
#define	SURF_ALPHASHADOW		0x10000	// do per-pixel light shadow casting in q3map
#define	SURF_NODLIGHT			0x20000	// never add dynamic lights
-}