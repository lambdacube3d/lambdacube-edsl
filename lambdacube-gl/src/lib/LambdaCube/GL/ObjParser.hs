{-# LANGUAGE OverloadedStrings #-}
module ObjParser where

import System.Environment
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import LambdaCube.GL

-- http://www.martinreddy.net/gfx/3d/OBJ.spec
{-
data ObjMesh
  = ObjMesh
  { points    :: [Int]
  , lines     :: [Int]
  , triangles :: [Int]
--  , name
  }
-}
data ObjState
  = ObjState
  { position      :: [V3F]
  , normal        :: [V3F]
  , uvw           :: [V3F]
  , positionSize  :: Int
  , normalSize    :: Int
  , uvwSize       :: Int
  , points        :: [Int]
  , lines'        :: [Int]
  , triangles     :: [Int]
  }
  deriving Show

readV3F [x] = V3 (read $ B.unpack x) 0 0
readV3F [x,y] = V3 (read $ B.unpack x) (read $ B.unpack y) 0
readV3F [x,y,z] = V3 (read $ B.unpack x) (read $ B.unpack y) (read $ B.unpack z)
readV3F l = error $ show l

incPosition = modify (\s -> s {positionSize = positionSize s + 1})
incNormal = modify (\s -> s {normalSize = normalSize s + 1})
incUVW = modify (\s -> s {uvwSize = uvwSize s + 1})

tokenize :: Bool -> ByteString -> [(Int, [ByteString])]
tokenize filterComments str = zip [1..] . map B.words . joinLines . (if filterComments then map filterComment else id) . B.lines $ str
  where
    filterComment = B.takeWhile (/= '#')
    joinLines (a:b:xs)
      | B.length a > 0 && B.last a == '\\' = joinLines (B.init a `B.append` b : xs)
      | otherwise = a : joinLines (b:xs)
    joinLines a = a

{-
TODO:
  support position parsing first
  parse lines
  parse tirangles
  parse quads
  add normal parsing
  add texture coord parsing
-}

readObj str = execState (mapM_ parseLine $ tokenize False str) $ ObjState [] [] [] 0 0 0 [] [] []
  where
    addV  xs = modify (\s -> s {position = readV3F xs : position s}) >> incPosition
    addVT xs = modify (\s -> s {uvw = readV3F xs : uvw s}) >> incUVW
    addVN xs = modify (\s -> s {normal = readV3F xs : normal s}) >> incNormal
    addP  xs = modify (\s -> s {points = xs ++ points s})
    addL  xs = modify (\s -> s {lines' = xs ++ lines' s})
    addT  xs = modify (\s -> s {triangles = xs ++ triangles s})
    chop "" = []
    chop xs = a : if b == "/" then [""] else chop (B.drop 1 b)
     where
      (a,b) = B.break (=='/') xs
    parseLine (lineNumber,l) = case l of
--      [] -> return ()
      -- * Supported statements
      -- Vertex data
      "v":xs -> case xs of
        [_,_,_]   -> addV xs
        [_,_,_,_] -> notSupported
        _ -> err
      ["vn",x,y,z] -> addVN [x,y,z]
      "vt":xs -> case xs of
        [_]     -> addVT xs
        [_,_]   -> addVT xs
        [_,_,_] -> addVT xs
        _ -> err
      -- Elements
      "p":xs -> do
        pIndex <- gets positionSize
        let f x = if x < 0 then x + pIndex else x - 1
        addP $ map (f . read . B.unpack) xs
      "l":xs -> do
        pIndex <- gets positionSize
        let f x = if x < 0 then x + pIndex else x - 1
            ps = map (f . read . B.unpack . head . chop) xs
            segments (a:xs@(b:_)) = a : b : segments xs
            segments a = a
        addL $ segments ps
      "f":xs -> do
        pIndex <- gets positionSize
        let f x = if x < 0 then x + pIndex else x - 1
            ps = map (f . read . B.unpack . head . chop) xs
        case xs of
          [_,_,_]   -> addT ps
          [_,_,_,_] -> let [a,b,c,d] = ps in addT [a,b,c,c,d,a]
          _ -> notSupported
      -- Grouping
      "g":_ -> return () -- TODO
      ["s",_] -> return () -- TODO
      ["o",_] -> return () -- TODO
      -- Display/render attributes
      ["usemap",_] -> return () -- TODO
      ["usemtl",_] -> return () -- TODO
      "mtllib":_ -> return () -- TODO
      _ -> return ()
{-
      -- * Unsupported statements (for future work)
      -- Vertex data
      "vp":xs -> case xs of
        [_]     -> notSupported
        [_,_]   -> notSupported
        [_,_,_] -> notSupported
        _ -> err
      "cstype":xs -> case xs of
        ["rat",a] | a `elem` ["bmatrix","bezier","bspline","cardinal","taylor"] -> notSupported
        [a] | a `elem` ["bmatrix","bezier","bspline","cardinal","taylor"] -> notSupported
        _ -> err
      "deg":xs -> case xs of
        [_]   -> notSupported
        [_,_] -> notSupported
        _ -> err
      "bmat":xs -> case xs of
        "u":_ -> notSupported
        "v":_ -> notSupported
        _ -> err
      "step":xs -> case xs of
        [_]   -> notSupported
        [_,_] -> notSupported
        _ -> err
      -- Elements
      "curv":_ -> notSupported
      "curv2":_ -> notSupported
      "surf":_ -> notSupported
      -- Free-fvorm curve/surface body statements
      "parm":_ -> notSupported
      "trim":_ -> notSupported
      "hole":_ -> notSupported
      "scrv":_ -> notSupported
      "sp":_ -> notSupported
      ["end"] -> notSupported
      -- Connectivity between free-form surfaces
      ["con",_,_,_,_,_,_,_,_] -> notSupported
      -- Grouping
      ["mg",_,_] -> notSupported
      -- Display/render attributes
      "bevel":xs -> case xs of
        ["on"]  -> notSupported
        ["off"] -> notSupported
        _ -> err
      "c_interp":xs -> case xs of
        ["on"]  -> notSupported
        ["off"] -> notSupported
        _ -> err
      "d_interp":xs -> case xs of
        ["on"]  -> notSupported
        ["off"] -> notSupported
        _ -> err
      ["lod",_] -> notSupported
      ["shadow_obj",_]  -> notSupported
      ["trace_obj",_]   -> notSupported
      "ctech":xs -> case xs of
        ["cparm",_]     -> notSupported
        ["cspace",_]    -> notSupported
        ["curv",_,_]    -> notSupported
        _ -> err
      "stech":xs -> case xs of
        ["cparma",_,_]  -> notSupported
        ["cparmb",_]    -> notSupported
        ["cspace",_]    -> notSupported
        ["curv",_,_]    -> notSupported
        _ -> err
      (a : _)
        | B.head a == '#' -> return ()
        | otherwise -> err
-}
     where
      err = error $ "line " ++ show lineNumber ++ " syntax error: " ++ show l
      notSupported = error $ "line " ++ show lineNumber ++ " unsupported statement: " ++ show l

{-
newmtl my_mtl

Ka 0.0435 0.0435 0.0435
Kd 0.1086 0.1086 0.1086
Ks 0.0000 0.0000 0.0000
Tf 0.9885 0.9885 0.9885
illum 6
d -halo 0.6600
Ns 10.0000
sharpness 60
Ni 1.19713

map_Ka -s 1 1 1 -o 0 0 0 -mm 0 1 chrome.mpc
map_Kd -s 1 1 1 -o 0 0 0 -mm 0 1 chrome.mpc
map_Ks -s 1 1 1 -o 0 0 0 -mm 0 1 chrome.mpc
map_Ns -s 1 1 1 -o 0 0 0 -mm 0 1 wisp.mps
map_d -s 1 1 1 -o 0 0 0 -mm 0 1 wisp.mps
disp -s 1 1 .5 wisp.mps
decal -s 1 1 1 -o 0 0 0 -mm 0 1 sand.mps
bump -s 1 1 1 -o 0 0 0 -bm 1 sand.mpb

refl -type sphere -mm 0 1 clouds.mpc

newmtl name

Ka r g b
Ka spectral file.rfl factor
Ka xyz x y z

Kd r g b
Kd spectral file.rfl factor
Kd xyz x y z

Ks r g b
Ks spectral file.rfl factor
Ks xyz x y z

Tf r g b
Tf spectral file.rfl factor
Tf xyz x y z

d factor
d -halo factor

Ns exponent

sharpness value

Ni optical_density

map_Ka -options args filename
map_Kd -options args filename
map_Ks -options args filename
map_Ns -options args filename

map_d -options args filename

map_aat on
decal -options args filename

disp -options args filename
bump -options args filename
-}

{-
pragmatic:

Wavefront OBJ material file
#this is a comment
Ka 0.00941176 0.0745098 0.0156863
Ka 0.14 0.45 0.091 # Green
Kd 0.000000 0.000000 0.501961
Ke 0.3 0.05 0.01
Ke 17 12 4# The empty water Cornell Box as seen in Henrik Jensen's
Ks .0001 .0001 .0001
Ks 0 0 0
Ks 0.427451 0.427451 0.427451
Ni 1.0000
Ns 0
Tf 0.066667 0.631373 0.074510
Tr 0
Tr 0 0
bump kamen-bump.png
bump textures\vase_round_bump.png
d 0.100000
illum 2
illum 4
illum 5
illum 5
illum 7
map_Ka KAMEN-stup.png
map_Ka textures\background.tga
map_Kd KAMEN-stup.png
map_Kd textures\sponza_floor_a_diff.tga
map_bump -bm 0.001 bump-lowRes.png
map_bump kamen-bump.png
map_bump textures\vase_round_bump.png
map_d house-Alpha.png
map_d textures\vase_plant_mask.tga
newmtl Brick
-}

data Mtl
  = Mtl
  { mtlName     :: String
  , mtlKa       :: V3F
  , mtlKd       :: V3F
  , mtlKe       :: V3F
  , mtlKs       :: V3F
  , mtlNi       :: Float
  , mtlTf       :: V3F
  , mtlBump     :: String
  , mtlD        :: Float
  , mtlIllum    :: Int
  , mtlMapKa    :: String
  , mtlMapKd    :: String
  , mtlMapBump  :: String
  , mtlMapBumpMultiplier :: Float
  , mtlMapD     :: String
  }

readMtl str = execState (mapM_ parseLine $ tokenize True str) () -- $ ObjState [] [] [] 0 0 0
  where
    parseLine (1,["Wavefront","OBJ","material","file"]) = return ()
    parseLine (lineNumber,l) = case l of
      [] -> return ()
      ["newmtl",a] -> return ()
      ["Ka",r,g,b] -> return ()
      ["Kd",r,g,b] -> return ()
      ["Ke",r,g,b] -> return ()
      ["Ks",r,g,b] -> return ()
      ["Ni",a] -> return ()
      ["Ns",a] -> return ()
      ["Tf",r,g,b] -> return ()
      -- ["Tr",a] -> return ()
      -- ["Tr",a,b] -> return ()
      ["bump",a] -> return ()
      ["d",a] -> return ()
      ["illum",a] -> return ()
      ["map_Ka",a] -> return ()
      ["map_Kd",a] -> return ()
      ["map_bump","-bm",a,b] -> return ()
      ["map_bump",a] -> return ()
      ["map_d",a] -> return ()
      _ -> return ()
     where
      err = error $ "line " ++ show lineNumber ++ " syntax error: " ++ show l
      notSupported = error $ "line " ++ show lineNumber ++ " unsupported statement: " ++ show l

main = do
  l <- getArgs
  forM_ l $ \n -> do
    print n
    c <- B.readFile n
    print $ length $ triangles $ readObj c
