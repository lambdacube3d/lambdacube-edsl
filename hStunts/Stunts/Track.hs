{-# LANGUAGE ParallelListComp, OverloadedStrings #-}

module Stunts.Track(terrainModelMap,trackModelMap,trackModelSizeMap) where

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IM
import Data.ByteString.Char8 (ByteString)

trackOrientation :: [a] -> [Float]
trackOrientation l
    | n == 2 = [0, 3*a]
    | n == 4 = [0, a, 2*a, 3*a]
    | otherwise = [0]
  where
    a = pi / 2
    n = length l

terrainOrientation :: [Float]
terrainOrientation = [0, a, 2*a, 3*a]
  where
    a = pi / 2

type ModelID = (ByteString,ByteString) -- FileName ModelName
type ModelMap = IntMap ([ModelID],[ModelID],Float) -- Graphics, Collision, Orientation

meshMap :: ByteString -> [ByteString] -> [(ByteString,ByteString)]
meshMap r l = [(r,n) | n <- l]

trackModels :: IntMap ([ModelID],[ModelID])
trackModels = IM.map (\(a,b) -> (meshMap "GAME1.P3S" a,meshMap "GAME1.P3S" b)) trackModelsGame1P3S `IM.union`
              IM.map (\(a,b) -> (meshMap "GAME2.P3S" a,meshMap "GAME2.P3S" b)) trackModelsGame2P3S

terrainModels :: IntMap ([ModelID],[ModelID])
terrainModels = IM.map (\l -> let ml = meshMap "GAME2.P3S" l in (ml,ml)) terrainModelsGame2P3S

addModel :: [Float] -> IntMap ([ModelID],[ModelID]) -> [Int] -> [(Int,([ModelID],[ModelID],Float))]
addModel ol m l@ ~(i0:_) = [(i,(ma,mb,o)) | i <- l | o <- ol]
  where
    (ma,mb) = m ! i0

addMultiMaterialModel :: [Float] -> IntMap ([ModelID],[ModelID]) -> [(Int,Int,Int)] -> [(Int,([ModelID],[ModelID],Float))]
addMultiMaterialModel ol m l@ ~((i0,_,_):_) = concat [[(a,(ma,mb,o)), (b,(ma,mb,o)), (c,(ma,mb,o))] | (a,b,c) <- l | o <- ol]
  where
    (ma,mb) = m ! i0

trackModelMap :: ModelMap
trackModelMap = IM.fromList $ concat $
                [addModel (trackOrientation m) trackModels m | m <- track] ++
                [addMultiMaterialModel (trackOrientation m) trackModels m | m <- variableMaterialTrack]

terrainModelMap :: ModelMap
terrainModelMap = IM.fromList $ addModel terrainOrientation terrainModels =<< terrain

-- rotate the list items, the first item will satisfy the given condition
sortFun :: (a -> Bool) -> [a] -> [a]
sortFun g l = take n $ dropWhile g $ take (2*n) $ cycle l
  where n = length l

-- anti-clockwise rotation from left to right
terrain :: [[Int]]
terrain = map (sortFun (`IM.notMember` terrainModels))
    [ [0x00]
    , [0x01]
    , [0x05, 0x02, 0x03, 0x04]
    , [0x06]
    , [0x07, 0x08, 0x09, 0x0A]
    , [0x0B, 0x0C, 0x0D, 0x0E]
    , [0x11, 0x12, 0x0F, 0x10]
    ]

--GAME2.P3S
terrainModelsGame2P3S :: IntMap [ByteString]
terrainModelsGame2P3S = IM.fromList
    [ (0x0F, ["goui"])
    , (0x0B, ["gouo", "high"]) -- needs a ground square
    , (0x07, ["goup"])
    , (0x01, ["lake"])
    , (0x00, ["high"])
    , (0x06, ["high"])
    , (0x02, ["lakc", "high"]) -- 0x02 or 0x04 Terrain ???
    ]
{-
--GAME2.P3S
,"hig1" - ??? nagyobb mint egy negyzet 0x06 Terrain ???
,"hig2" - ??? nagyobb mint egy negyzet 0x ???
,"hig3" - ??? nagyobb mint egy negyzet 0x ???
-}

track :: [[Int]]
track = map (sortFun (`IM.notMember` trackModels))
    [ [0x00]
    , [0x22, 0x23]
    , [0x63, 0x64]
    , [0x65, 0x66]
    , [0x67, 0x68]
    , [0x26, 0x25, 0x27, 0x24]
    , [0x3A, 0x39, 0x3B, 0x38]
    , [0x61, 0x60, 0x62, 0x5F]
    , [0x75, 0x76, 0x77, 0x78]
    , [0x79, 0x7A, 0x7B, 0x7C]
    , [0x40, 0x41]
    , [0x55, 0x56]
    , [0x42, 0x43]
    , [0x73, 0x74]
    , [0x44, 0x45]
    , [0x53, 0x54]
    , [0x46, 0x48, 0x47, 0x49]
    , [0x6D, 0x6E]
    , [0x6F, 0x72, 0x71, 0x70]
    , [0x32, 0x30, 0x33, 0x31]
    , [0x34, 0x36, 0x37, 0x35]
    , [0x69, 0x6B, 0x6C, 0x6A]
    , [0x3C, 0x3F]
    , [0x3D, 0x3E]
    , [0x28, 0x2B, 0x2A, 0x29]
    , [0x2F, 0x2E, 0x2D, 0x2C]
    , [0x02]
    , [0x03]
    , [0x97]
    , [0x98]
    , [0x99]
    , [0x9A]
    , [0x9B, 0x9D, 0x9C, 0x9E]
    , [0x9F, 0xA1, 0xA0, 0xA2]
    , [0xA3, 0xA5, 0xA4, 0xA6]
    , [0xA7, 0xA9, 0xA8, 0xAA]
    , [0xAD, 0xAC, 0xAE, 0xAB]
    , [0xAF, 0xB1, 0xB0, 0xB2]
    , [0x5B, 0x5C, 0x5D, 0x5E]
    , [0x58, 0x59, 0x5A, 0x57]
    ]

-- (paved, dirt, icy)
variableMaterialTrack :: [[(Int,Int,Int)]]
variableMaterialTrack = map (sortFun (\(i,_,_) -> IM.notMember i trackModelsGame1P3S && IM.notMember i trackModelsGame2P3S)) $
    map (\(a,b,c) -> zip3 a b c)
    [ ([0x01, 0xB5, 0xB3, 0xB4],    [0x86, 0x89, 0x87, 0x88],   [0x93, 0x96, 0x94, 0x95])
    , ([0x04, 0x05],                [0x0E, 0x0F],               [0x18, 0x19])
    , ([0x4A],                      [0x7D],                     [0x8A])
    , ([0x06, 0x08, 0x09, 0x07],    [0x10, 0x12, 0x13, 0x11],   [0x1A, 0x1C, 0x1D, 0x1B])
    , ([0x0A, 0x0C, 0x0D, 0x0B],    [0x14, 0x16, 0x17, 0x15],   [0x1E, 0x20, 0x21, 0x1F])
    , ([0x4F, 0x50, 0x51, 0x52],    [0x82, 0x83, 0x84, 0x85],   [0x8F, 0x90, 0x91, 0x92])
    , ([0x4C, 0x4D, 0x4E, 0x4B],    [0x7F, 0x80, 0x81, 0x7E],   [0x8C, 0x8D, 0x8E, 0x8B])
    -- special element
    , ([0xD0, 0xD1, 0xD2, 0xD3],    [0xD4, 0xD5, 0xD6, 0xD7],   [0xD8, 0xD9, 0xDA, 0xDB])
    ]

trackModelSizeMap :: IntMap (Int,Int)
trackModelSizeMap = IM.union sizeMap $ IM.map (const (1,1)) trackModelMap
  where
    sizeMap = IM.fromList [(i,s) | (is,s) <- [(s2x2,(2,2)),(s1x2,(1,2)),(s2x1,(2,1))], i <- is]
    s2x2 =
      [ 0x0A, 0x0B, 0x0C, 0x0D
      , 0x14, 0x15, 0x16, 0x17
      , 0x1E, 0x1F, 0x20, 0x21
      , 0x34, 0x35, 0x36, 0x37
      , 0x3C, 0x3D, 0x3E, 0x3F
      , 0x57, 0x58, 0x59, 0x5A
      , 0x5B, 0x5C, 0x5D, 0x5E
      , 0x69, 0x6A, 0x6B, 0x6C
      , 0x75, 0x76, 0x77, 0x78
      , 0x79, 0x7A, 0x7B, 0x7C
      ]
    s1x2 = [ 0x40, 0x55 ]
    s2x1 = [ 0x41, 0x56 ]

trackModelsGame1P3S :: IntMap ([ByteString],[ByteString])
trackModelsGame1P3S = IM.fromList
    -- simple
    [ (0x31, (["bank"], ["zban"]))
    , (0x3E, (["chi1"], ["zch1"]))
    , (0x3C, (["chi2"], ["zch2"]))
    , (0x6F, (["gwro"], ["zgwr"]))
    , (0x53, (["hpip"], ["zhpi"]))
    , (0x4A, (["inte"], ["zint"]))
    , (0x2C, (["lban"], ["zlba"]))
    , (0xA3, (["offi"], ["zoff"]))
    , (0x4B, (["offl"], ["zofl"]))
    , (0x4F, (["offr"], ["zofr"]))
    , (0x28, (["rban"], ["zrba"]))
    , (0x57, (["sofl"], ["zsol"]))
    , (0x5B, (["sofr"], ["zsor"]))
    , (0x46, (["spip"], ["zspi"]))
    , (0x0B, (["stur"], ["zstu"]))
    , (0x07, (["turn"], ["ztur"]))
    , (0x6D, (["wroa"], ["zwro"]))
    , (0x04, (["road"], ["zroa"]))

    -- complex
    , (0x42, (["tunn", "tun2"],  ["ztun"]))
    , (0x35, (["btur"],          ["zbtu"]))
    , (0x01, (["road", "fini"],  ["zroa", "zfin"]))
    , (0x40, (["loo1", "loop"],  ["zloo"]))
    , (0x44, (["pip2", "pipe"],  ["zpip"]))
    , (0x55, (["vcor"],          ["zvco"])) -- milyen ut van alatta?
    , (0x73, (["barr", "road"],  ["zbar"])) -- jo az utkozes geometria? nem jo az akadaly utkozes es nincs ut utkozes modell
    ]
{-
todo:
,"cfen" - kerites sarok
,"zcfe" - kerites sarok utkozes modell

,"fenc" - kerites oldal
,"zfen" - kerites oldal utkozes modell
]
-}

trackModelsGame2P3S :: IntMap ([ByteString],[ByteString])
trackModelsGame2P3S = IM.fromList
    -- simple
    [ (0xAB, (["boat"], ["zboa"]))
    , (0x3A, (["brid"], ["zbri"]))
    , (0x22, (["elrd"], ["zelr"]))
    , (0x67, (["elsp"], ["zesp"]))
    , (0x9B, (["gass"], ["zgas"]))
    , (0x97, (["palm"], ["zpal"]))
    , (0x26, (["ramp"], ["zram"]))
    , (0xAF, (["rest"], ["zres"]))
    , (0x63, (["selr"], ["zser"]))
    , (0x6A, (["sest"], ["zses"]))
    , (0x61, (["sram"], ["zsra"]))
    , (0xA7, (["wind"], ["zwin"]))
    , (0x98, (["cact"], ["cact"]))
    , (0x99, (["tree"], ["tree"]))
    , (0x9A, (["tenn"], ["zten"]))
    , (0x9F, (["barn"], ["zbrn"]))

    -- complex
    , (0x75, (["lco0", "lco1"], ["zlco"]))
    , (0x79, (["rco0", "rco1"], ["zrco"]))

    -- special element
        -- road on 0x09 Terrain
    , (0xD0, (["rdup"], ["zrdu"]))
    ]
-- egy palyaelem alatt sincs terrain
{-
todo:
,"cld1" - 0x ??? felho
,"cld2" - 0x ??? felho
,"cld3" - 0x ??? felho
,"exp0" - 0x ???
,"exp1" - 0x ???
,"exp2" - 0x ???
,"exp3" - 0x ???
,"flag" - 0x ???

,"sigl" - 0x ??? bal kanyar jelzo tabla (tukrozni kell, fel kell cserelni az eszak <-> del iranyt)
,"sigr" - 0x ??? jobb kanyar jelzo tabla (tukrozni kell, fel kell cserelni az eszak <-> del iranyt)

,"truk" - 0x eszak fele nezo teherauto, innen gurul ki a veresenyzo kocsija
]
-}

