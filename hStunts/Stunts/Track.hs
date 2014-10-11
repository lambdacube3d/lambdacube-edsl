{-# LANGUAGE OverloadedStrings #-}
module Stunts.Track
    ( terrainModelMap
    , trackModelMap
    , trackModelSizeMap
    ) where

import Control.Arrow
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.IntMap as IM
import Data.ByteString.Char8 (ByteString)

type FileName  = ByteString
type ModelName = ByteString
type ModelID   = (FileName, ModelName)
type Item      = ([ModelID], [ModelID], Float) -- Graphics, Collision, Orientation
type Size      = (Int, Int)

{- TODO
"cfen" - kerites sarok
"zcfe" - kerites sarok utkozes modell
"fenc" - kerites oldal
"zfen" - kerites oldal utkozes modell

"cld1" - 0x ??? felho
"cld2" - 0x ??? felho
"cld3" - 0x ??? felho
"exp0" - 0x ???
"exp1" - 0x ???
"exp2" - 0x ???
"exp3" - 0x ???
"flag" - 0x ???

"sigl" - 0x ??? bal kanyar jelzo tabla (tukrozni kell, fel kell cserelni az eszak <-> del iranyt)
"sigr" - 0x ??? jobb kanyar jelzo tabla (tukrozni kell, fel kell cserelni az eszak <-> del iranyt)

"truk" - 0x eszak fele nezo teherauto, innen gurul ki a veresenyzo kocsija

"hig1" - ??? nagyobb mint egy negyzet 0x06 Terrain ???
"hig2" - ??? nagyobb mint egy negyzet 0x ???
"hig3" - ??? nagyobb mint egy negyzet 0x ???

-- egy palyaelem alatt sincs terrain
-}

terrainModelMap :: IM.IntMap Item
terrainModelMap = IM.map fst $ runBuilder $ do

    setFileName "GAME2.P3S"

    items_ [0x01] ["lake"]
    items_ [0x00] ["high"]
    items_ [0x06] ["high"]
    items_ (x4 0x0F) ["goui"]
    items_ (x4 0x0B) ["gouo", "high"] -- needs a ground square
    items_ (x4 0x07) ["goup"]
    items_ (x4 0x02) ["lakc", "high"] -- 0x02 or 0x04 Terrain ???

trackModelMap     :: IM.IntMap Item
trackModelSizeMap :: IM.IntMap Size
(trackModelMap, trackModelSizeMap) = IM.map fst &&& IM.map snd $ runBuilder $ do

    setFileName "GAME1.P3S"

    setSize (1,1)
    items [0x31, 0x32, 0x30, 0x33] ["bank"] ["zban"]
    items [0x6F, 0x72, 0x71, 0x70] ["gwro"] ["zgwr"]
    items (x2 0x53) ["hpip"] ["zhpi"]
    items [0x2C, 0x2F, 0x2E, 0x2D] ["lban"] ["zlba"]
    items (x4'' 0xA3) ["offi"] ["zoff"]
    items [0x28, 0x2B, 0x2A, 0x29] ["rban"] ["zrba"]
    items (x4'' 0x46) ["spip"] ["zspi"]
    items (x2 0x6D) ["wroa"] ["zwro"]
    items (x2 0x42) ["tunn", "tun2"] ["ztun"]
    items (x2 0x44) ["pip2", "pipe"] ["zpip"]
    items (x2 0x73) ["barr", "road"] ["zbar"] -- jo az utkozes geometria? nem jo az akadaly utkozes es nincs ut utkozes modell
    multiItems ([0x4A], [0x7D], [0x8A]) ["inte"] ["zint"]
    multiItems (x4 0x4B, x4 0x7E, x4 0x8B) ["offl"] ["zofl"]
    multiItems (x4 0x4F, x4 0x82, x4 0x8F) ["offr"] ["zofr"]
    multiItems (x4' 0x06, x4' 0x10, x4' 0x1A) ["turn"] ["ztur"]
    multiItems (x2 0x04, x2 0x0E, x2 0x18) ["road"] ["zroa"]
    multiItems ([0x01, 0xB5, 0xB3, 0xB4], [0x86, 0x89, 0x87, 0x88], [0x93, 0x96, 0x94, 0x95]) ["road", "fini"] ["zroa", "zfin"]

    setSize (1,2)
    items (x2 0x40) ["loo1", "loop"] ["zloo"]
    items (x2 0x55) ["vcor"] ["zvco"] -- milyen ut van alatta?

    setSize (2,2)
    items [0x3C, 0x3F] ["chi2"] ["zch2"]
    items [0x3E, 0x3D] ["chi1"] ["zch1"]
    items (x4 0x57) ["sofl"] ["zsol"]
    items (x4 0x5B) ["sofr"] ["zsor"]
    items (x4' 0x34) ["btur"] ["zbtu"]
    multiItems (x4' 0x0A, x4' 0x14, x4' 0x1E) ["stur"] ["zstu"]

    setFileName "GAME2.P3S"

    setSize (1,1)
    items (x4'' 0xAB) ["boat"] ["zboa"]
    items [0x3A, 0x39, 0x3B, 0x38] ["brid"] ["zbri"]
    items [0x22, 0x23] ["elrd"] ["zelr"]
    items [0x67, 0x68] ["elsp"] ["zesp"]
    items (x4'' 0x9B) ["gass"] ["zgas"]
    items [0x97] ["palm"] ["zpal"]
    items [0x26, 0x25, 0x27, 0x24] ["ramp"] ["zram"]
    items (x4'' 0xAF) ["rest"] ["zres"]
    items (x2 0x63) ["selr"] ["zser"]
    items [0x61, 0x60, 0x62, 0x5F] ["sram"] ["zsra"]
    items (x4'' 0xA7) ["wind"] ["zwin"]
    items [0x98] ["cact"] ["cact"]
    items [0x99] ["tree"] ["tree"]
    items [0x9A] ["tenn"] ["zten"]
    items (x4'' 0x9F) ["barn"] ["zbrn"]
    multiItems (x4 0xD0, x4 0xD4, x4 0xD8) ["rdup"] ["zrdu"]  -- special element, road on 0x09 Terrain

    setSize (2,2)
    items (x4' 0x69) ["sest"] ["zses"]
    items (x4 0x75) ["lco0", "lco1"] ["zlco"]
    items (x4 0x79) ["rco0", "rco1"] ["zrco"]

type Key = Int

x2, x4, x4', x4'' :: Key -> [Key]
x2   i = [i,i+1]
x4   i = [i..i+3]
x4'  i = [i+1, i, i+2, i+3]
x4'' i = [i, i+2, i+1, i+3]

type Builder a = StateT (FileName, Size) (Writer (IM.IntMap a))

runBuilder :: Builder a () -> IM.IntMap a
runBuilder = snd . runWriter . flip evalStateT (error "filename missing", error "size missing")

setSize :: Size -> Builder a ()
setSize s = modify (id *** const s)

setFileName :: FileName -> Builder a ()
setFileName n = modify (const n *** id)

items :: [Key] -> [ModelName] -> [ModelName] -> Builder (Item, Size) ()
items ks graphics collision
    = mapM_ item $ zip ks $ orientations $ length ks
  where
    orientations 2 = [0,3]
    orientations _ = [0..]

    item (key, quarterturn) = do
        (filename, size) <- get
        lift $ tell $ IM.singleton key
          ( ( map ((,) filename) graphics
            , map ((,) filename) collision
            , fromIntegral quarterturn * (pi/2)
            )
          , if even quarterturn then size else snd &&& fst $ size
          )

items_ :: [Key] -> [ModelName] -> Builder (Item, Size) ()
items_ = join . items

multiItems :: ([Key], [Key], [Key]) -> [ModelName] -> [ModelName] -> Builder (Item, Size) ()
multiItems (paved, dirt, icy) gr coll = do
    items paved gr coll
    items dirt  gr coll
    items icy   gr coll
