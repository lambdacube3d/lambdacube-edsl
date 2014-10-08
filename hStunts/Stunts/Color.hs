module Stunts.Color
    ( Material (..)
    , Pattern (..)
    , Color
    , serializeColor
    , materialMap
    , vgaPal
    ) where

import Data.Bits
import qualified Data.Vector as V
import Data.Word

data Pattern
    = Opaque
    | Grate
    | Transparent
    | Grille
    | InverseGrille
    | Glass
    | InverseGlass

data Material
    = Material
    { pattern    :: Pattern
    , color      :: Word32
    , colorIndex :: Int
    , shininess  :: Float
    }

data Color
    = Color Word8   -- red
            Word8   -- green
            Word8   -- blue
            Word8   -- alpha

serializeColor :: Color -> [Word8]
serializeColor (Color r g b a) = [r, g, b, a]

toRGBA :: Word32 -> Color
toRGBA 0xFFFFFF = Color 0 0 0 0
toRGBA c = Color (f 16) (f 8) (f 0) 0xFF
  where
    f i = fromIntegral $ (c `shiftR` i) .&. 0xFF

{-
checkFun m = vgaPal IM.! (colorIndex m) == (toRGB $ color m)
check = foldl (&&) True $ IM.elems $ IM.map checkFun materialMap
-}

--  MaterialIndex      Pattern        Color     ColorIndexOnPalette
materialMap :: V.Vector Material
materialMap = V.fromList
    -- 0
    [ Material Opaque         0x000000    0   4.0 -- Black
    , Material Opaque         0x0000A8    1   4.0 -- Blue
    , Material Opaque         0x00A800    2   4.0 -- Green
    , Material Opaque         0x00A8A8    3   4.0
    , Material Opaque         0xA80000    4   4.0 -- Highway light / cork l/r wall
    , Material Opaque         0xA800A8    5   4.0
    , Material Opaque         0xA85400    6  20.0 -- Barn roof
    , Material Opaque         0xA8A8A8    7   4.0 -- Gas station doors
    , Material Opaque         0x545454    8   4.0
    , Material Opaque         0x5454FC    9   4.0
    -- 10
    , Material Opaque         0x54FC54   10   4.0 -- Tree (pine)
    , Material Opaque         0x54FCFC   11   4.0
    , Material Opaque         0xFC5454   12   4.0 -- Highway split light
    , Material Opaque         0xFC54FC   13   4.0
    , Material Opaque         0xFCFC54   14   4.0 -- Sharp turn sign
    , Material Opaque         0xFCFCFC   15   4.0 -- Tennis court lines
    , Material Opaque         0x246820  108   4.0 -- Grass
    , Material Opaque         0x5CFCFC  116   0.0 -- Sky
    , Material Opaque         0xFCFCFC   15  20.0 -- Highway centerline
    , Material Opaque         0x484848   28  20.0 -- Asphalt pavement
    -- 20
    , Material Opaque         0x383838   29  20.0 -- Tunnel centerline
    , Material Opaque         0xFCFC54   14  20.0 -- Asphalt centerline
    , Material Grate          0x484848   28  15.0 -- Loop / Elev. road surface
    , Material Grate          0x202020   31  15.0 -- Pipe surface
    , Material Grate          0xFCFC54   14  15.0
    , Material Opaque         0x9C6438  200   4.0 -- Dirt pavement
    , Material Opaque         0xB48054  198   4.0
    , Material Opaque         0xCCA080  196   4.0 -- Dirt centerline
    , Material Opaque         0xD8FCFC  112   4.0 -- Ice pavement
    , Material Opaque         0x9CFCFC  114   4.0
    -- 30
    , Material Opaque         0x5CFCFC  116   4.0 -- Ice centerline
    , Material Opaque         0xE4C4AC  194   4.0 -- Bridges, buildings, etc.
    , Material Opaque         0xC0906C  197   4.0 -- Bridges, buildings, etc.
    , Material Opaque         0x9C6438  200   4.0 -- Bridges, buildings, etc.
    , Material Grate          0x9C9CFC  146   4.0
    , Material Opaque         0xFC4040   37  15.0 -- Banked road outer side
    , Material Opaque         0xFC7C7C   35  15.0 -- Tunnel walls / Bankings
    , Material Opaque         0xFC40FC  181   4.0 -- Bridge cables
    , Material Opaque         0x383838   29  20.0 -- Wheel (tyre tread)
    , Material Opaque         0x202020   31  20.0 -- Wheel (tyre sidewall)
    -- 40
    , Material Opaque         0xC0C0C0   19  40.0 -- Wheel ("rim")
    , Material Opaque         0x00A8A8    3  40.0 -- Windows (LM002 car1)
    , Material Opaque         0x54FCFC   11  40.0 -- Gas station windows
    , Material Opaque         0x545454    8  40.0 -- Windows and windshields
    , Material Opaque         0x000000    0  40.0 -- Windows and windshields
    , Material Opaque         0xA80000    4   4.0
    , Material Opaque         0xA80000    4   4.0
    , Material Opaque         0xFC5454   12   4.0
    , Material Opaque         0x00007C  156  30.0 -- Blue 1      Car body
    , Material Opaque         0x0000A8    1  30.0 -- Blue 2      Car body
    -- 50
    , Material Opaque         0x0000D0  152  30.0 -- Blue 3      Car body
    , Material Opaque         0x0004FC  150  30.0 -- Blue 4      Car body
    , Material Opaque         0xB40000   42  30.0 -- Red 1       Car body
    , Material Opaque         0xE40000   40  30.0 -- Red 2       Car body
    , Material Opaque         0xFC2020   38  30.0 -- Red 3       Car body
    , Material Opaque         0xFC4040   37  30.0 -- Red 4       Car body
    , Material Opaque         0x545454    8  30.0 -- Graphite 1  Car body
    , Material Opaque         0x606060   26  30.0 -- Graphite 2  Car body
    , Material Opaque         0x707070   25  30.0 -- Graphite 3  Car body
    , Material Opaque         0x7C7C7C   24  30.0 -- Graphite 4  Car body
    -- 60
    , Material Opaque         0xE4D800   72  30.0 -- Yellow 1    Car body
    , Material Opaque         0xFCF420   70  30.0 -- Yellow 2    Car body
    , Material Opaque         0xFCF85C   68  30.0 -- Yellow 3    Car body
    , Material Opaque         0xFCFC9C   66  30.0 -- Yellow 4    Car body
    , Material Opaque         0x009C9C  123  30.0 -- Cyan 1      Car body
    , Material Opaque         0x00CCCC  121  30.0 -- Cyan 2      Car body
    , Material Opaque         0x00E4E4  120  30.0 -- Cyan 3      Car body
    , Material Opaque         0x40FCFC  117  30.0 -- Cyan 4      Car body
    , Material Opaque         0x508400   92  30.0 -- Green 1     Car body / Tree (palm)
    , Material Opaque         0x74B400   90  30.0 -- Green 2     Car body / Tree (palm)
    -- 70
    , Material Opaque         0x90E400   88  30.0 -- Green 3     Car body / Tree (palm)
    , Material Opaque         0xA0FC00   87  30.0 -- Green 4     Car body
    , Material Opaque         0x440070  173  30.0 -- Purple 1    Car body
    , Material Opaque         0x60009C  171  30.0 -- Purple 2    Car body
    , Material Opaque         0x8000CC  169  30.0 -- Purple 3    Car body
    , Material Opaque         0xA800FC  167  30.0 -- Purple 4    Car body
    , Material Opaque         0xB0B0B0   20  30.0 -- Silver 1    Car body
    , Material Opaque         0xC0C0C0   19  30.0 -- Silver 2    Car body
    , Material Opaque         0xCCCCCC   18  30.0 -- Silver 3    Car body
    , Material Opaque         0xDCDCDC   17  30.0 -- Silver 4    Car body
    -- 80
    , Material Opaque         0x6C5800   77  30.0 -- Golden 1    Car body
    , Material Opaque         0x847400   76  30.0 -- Golden 2    Car body
    , Material Opaque         0xB4A400   74  30.0 -- Golden 3    Car body
    , Material Opaque         0xCCC000   73  30.0 -- Golden 4    Car body
    , Material Opaque         0x700000   45  30.0 -- Burgundy 1  Car body
    , Material Opaque         0x840000   44  30.0 -- Burgundy 2  Car body
    , Material Opaque         0xB40000   42  30.0 -- Burgundy 3  Car body
    , Material Opaque         0xCC0000   41  30.0 -- Burgundy 4  Car body
    , Material Opaque         0x000040  159  30.0 -- Violet 1    Car body
    , Material Opaque         0x280040  175  30.0 -- Violet 2    Car body
    -- 90
    , Material Opaque         0x340058  174  30.0 -- Violet 3    Car body
    , Material Opaque         0x500084  172  30.0 -- Violet 4    Car body
    , Material Opaque         0x383838   29   4.0
    , Material Opaque         0x484848   28   4.0
    , Material Grate          0xCCCCCC   18   4.0 -- Tennis net
    , Material Opaque         0x74B400   90   4.0 -- Tennis grass
    , Material Opaque         0xFCFCFC   15   0.0 -- Clouds
    , Material Opaque         0xA8A8A8    7   0.0 -- Clouds
    , Material Opaque         0x9C6438  200   4.0 -- Pinetree trunk
    , Material Opaque         0xC0602C  219   4.0 -- Pinetree trunk
    -- 100
    , Material Opaque         0x0080D0  136  50.0 -- Water
    , Material Opaque         0x84E084   99   4.0 -- Grass (hilltop)
    , Material Opaque         0x70C46C  101   4.0 -- Grass (hill slope)
    , Material Opaque         0x58A858  103   4.0 -- Grass (angled slope)
    , Material Opaque         0x509C4C  104   4.0 -- Grass (angled slope)
    , Material Opaque         0x388034  106   4.0 -- Grass (angled slope)
    , Material Opaque         0xDCDCDC   17   4.0 -- Gas station / Joe's
    , Material Opaque         0xB0B0B0   20   4.0 -- Gas station / Joe's
    , Material Opaque         0x905410   60   4.0 -- Trunk (palmtree)
    , Material Opaque         0x6C5800   77   4.0 -- Trunk (palmtree)
    -- 110
    , Material Opaque         0x580000   46  30.0 -- Joe's roof
    , Material Opaque         0x744008   61   4.0 -- Windmill (base)
    , Material Opaque         0x700000   45   4.0 -- Windmill (base)
    , Material Opaque         0x80542C  202  20.0 -- Windmill (blades)
    , Material Opaque         0x540054  190  20.0 -- Joe's flashing sign
    , Material Opaque         0xA400A4  186  20.0 -- Joe's flashing sign
    , Material Opaque         0xE000E4  183  20.0 -- Joe's flashing sign
    , Material Opaque         0xFC5CFC  180  20.0 -- Joe's flashing sign
    , Material Transparent    0xFFFFFF  255   0.0 -- Windmill animation mask
    , Material Grille         0x484848   28   4.0
    -- 120
    , Material InverseGrille  0x2C2C2C   30   4.0
    , Material Glass          0xFCFCFC   15   4.0
    , Material InverseGlass   0xB0B0B0   20   4.0
    , Material Glass          0xFCF85C   68   4.0
    , Material InverseGlass   0xFCAC54   54   4.0
    , Material Glass          0xFC0000   39   4.0
    , Material InverseGlass   0x9C0000   43   4.0
    , Material Opaque         0xFC5454   12  20.0 -- Corner kerbs
    , Material Opaque         0xDCDCDC   17  20.0 -- Corner kerbs
    ]

vgaPal :: V.Vector Color
vgaPal = V.fromList $ map toRGBA [
    0x000000, 0x0000A8, 0x00A800, 0x00A8A8, 0xA80000, 0xA800A8, 0xA85400, 0xA8A8A8,
    0x545454, 0x5454FC, 0x54FC54, 0x54FCFC, 0xFC5454, 0xFC54FC, 0xFCFC54, 0xFCFCFC,
    0xFCFCFC, 0xDCDCDC, 0xCCCCCC, 0xC0C0C0, 0xB0B0B0, 0xA4A4A4, 0x989898, 0x888888,
    0x7C7C7C, 0x707070, 0x606060, 0x545454, 0x484848, 0x383838, 0x2C2C2C, 0x202020,
    0xFCD8D8, 0xFCB8B8, 0xFC9C9C, 0xFC7C7C, 0xFC5C5C, 0xFC4040, 0xFC2020, 0xFC0000,
    0xE40000, 0xCC0000, 0xB40000, 0x9C0000, 0x840000, 0x700000, 0x580000, 0x400000,
    0xFCE8D8, 0xFCDCC0, 0xFCD4AC, 0xFCC894, 0xFCC080, 0xFCB868, 0xFCAC54, 0xFCA43C,
    0xFC9C28, 0xE08820, 0xC4781C, 0xA86414, 0x905410, 0x744008, 0x583004, 0x402000,
    0xFCFCD8, 0xFCFCB8, 0xFCFC9C, 0xFCFC7C, 0xFCF85C, 0xFCF440, 0xFCF420, 0xFCF400,
    0xE4D800, 0xCCC000, 0xB4A400, 0x9C8C00, 0x847400, 0x6C5800, 0x544000, 0x402800,
    0xF8FCD8, 0xF4FCB8, 0xE8FC9C, 0xE0FC7C, 0xD0FC5C, 0xC4FC40, 0xB4FC20, 0xA0FC00,
    0x90E400, 0x80CC00, 0x74B400, 0x609C00, 0x508400, 0x447000, 0x345800, 0x284000,
    0xD8FCD8, 0x9CFC9C, 0x90EC90, 0x84E084, 0x78D078, 0x70C46C, 0x64B864, 0x58A858,
    0x509C4C, 0x449040, 0x388034, 0x2C742C, 0x246820, 0x185814, 0x0C4C08, 0x044000,
    0xD8FCFC, 0xB8FCFC, 0x9CFCFC, 0x7CFCF8, 0x5CFCFC, 0x40FCFC, 0x20FCFC, 0x00FCFC,
    0x00E4E4, 0x00CCCC, 0x00B4B4, 0x009C9C, 0x008484, 0x007070, 0x005858, 0x004040,
    0xD8ECFC, 0xB8E0FC, 0x9CD4FC, 0x7CC8FC, 0x5CBCFC, 0x40B0FC, 0x009CFC, 0x008CE4,
    0x0080D0, 0x0074BC, 0x0064A8, 0x005890, 0x004C7C, 0x003C68, 0x003054, 0x002440,
    0xD8D8FC, 0xB8BCFC, 0x9C9CFC, 0x7C80FC, 0x5C60FC, 0x4040FC, 0x0004FC, 0x0000E4,
    0x0000D0, 0x0000BC, 0x0000A8, 0x000090, 0x00007C, 0x000068, 0x000054, 0x000040,
    0xF0D8FC, 0xE4B8FC, 0xD89CFC, 0xD07CFC, 0xC85CFC, 0xBC40FC, 0xB420FC, 0xA800FC,
    0x9800E4, 0x8000CC, 0x7400B4, 0x60009C, 0x500084, 0x440070, 0x340058, 0x280040,
    0xFCD8FC, 0xFCB8FC, 0xFC9CFC, 0xFC7CFC, 0xFC5CFC, 0xFC40FC, 0xFC20FC, 0xE000E4,
    0xCC00CC, 0xB800B8, 0xA400A4, 0x900090, 0x7C007C, 0x680068, 0x540054, 0x400040,
    0xFCE8DC, 0xF0D4C4, 0xE4C4AC, 0xD8B498, 0xCCA080, 0xC0906C, 0xB48054, 0xAC7040,
    0x9C6438, 0x8C5C34, 0x80542C, 0x704C28, 0x604020, 0x54381C, 0x443014, 0x382810,
    0xFCD8CC, 0xF8CCB8, 0xF4C0A8, 0xF0B494, 0xECA884, 0xE89C74, 0xE49464, 0xE08C58,
    0xD8804C, 0xD47840, 0xC86C34, 0xC0602C, 0xB45424, 0xA8481C, 0x9C3C14, 0x94300C,
    0xF4C0A8, 0xF0BCA0, 0xF0B89C, 0xF0B494, 0xECB090, 0xECAC88, 0xECA884, 0xE8A480,
    0xE8A078, 0xE89C74, 0xE4986C, 0xE49468, 0xE49464, 0xFC9C9C, 0xFC9494, 0xFC9090,
    0xFC8C8C, 0xFC8484, 0xFC8080, 0xFC7C7C, 0xD8B498, 0xD0AC8C, 0xCCA484, 0xC89C7C,
    0xC49474, 0xC0906C, 0xC0C0C0, 0xBCBCBC, 0xB8B8B8, 0xB4B4B4, 0xB0B0B0, 0xFFFFFF
    ]
