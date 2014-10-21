{-# LANGUAGE OverloadedStrings #-}
module ObjParser where

import Control.Applicative
import Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8

-- http://www.martinreddy.net/gfx/3d/OBJ.spec

--newLine = inClass "\r\n"
newLine = notInClass "\r\n"

comment :: Parser ()
comment = mySkipSpace >> string "#" >> skipWhile newLine >> mySkipSpace -- >> return "this is a comment"

eol :: Parser ()
eol = comment <|> (skipWhile newLine >> mySkipSpace)

skipHSpace = mySkipSpace -- skipWhile $ inClass " \t"

mySkipSpace = skipWhile $ inClass " \t\r\n"

float :: Parser Double
float = skipHSpace >> double

int :: Parser Int
int = skipHSpace >> signed decimal

token :: Parser ByteString
token = skipHSpace >> takeWhile1 isAlpha_ascii

kw :: ByteString -> Parser ()
kw w = mySkipSpace >> string w *> skipHSpace *> return ()

-- Vertex data
geometricVertice = kw "v" *> float *> float *> float *> eol    -- v      -5.000000       5.000000       0.000000
textureVertice = kw "vt" *> float *> float *> option 0 float *> eol     -- vt     -5.000000       5.000000       0.000000
vertexNormal = kw "vn" *> float *> float *> float *> eol       -- vn      0.000000       0.000000       1.000000
parameterSpaceVertice = kw "vp" *> float *> float *> eol       -- vp      0.210000       3.590000
curveOrSurfaceType = kw "cstype" *> option False (kw "rat" *> return True) *> (kw "bmatrix" <|> kw "bezier" <|> kw "bspline" <|> kw "cardinal" <|> kw "taylor") *> eol
degree = kw "deg" *> float *> option 0 float *> eol
basisMatrix = kw "bmat" *> eol
stepSize = kw "step" *> decimal *> option 0 decimal *> eol

-- Elements
point = kw "p" *> many1 int *> eol
line = kw "l" *> many1 (int <* string "/" *> option 0 int) *> eol
face = kw "f" *> many1 (int <* option 0 (string "/" *> option 0 int <* string "/" *> option 0 int)) *> eol
curve = kw "curv" *> float *> float *> many1 int *> eol
curve2D = kw "curv2" *> many1 int *> eol
surface = kw "surf" *> float *> float *> float *> float *> many1 (int <* string "/" *> option 0 int <* string "/" *> option 0 int) *> eol

-- Free-fvorm curve/surface body statements
parameterValues = kw "parm" *> (kw "u" <|> kw "v") *> many1 float *> eol
outerTrimmingLoop = kw "trim" *> many1 (float *> float *> int) *> eol
innerTrimmingLoop = kw "hole" *> many1 (float *> float *> int) *> eol
specialCurve = kw "scrv" *> many1 (float *> float *> int) *> eol
specialPoint = kw "sp" *> many1 int *> eol
endStatement = kw "end" *> eol

-- Connectivity between free-form surfaces
connect = kw "con" *> int *> float *> float *> int *> int *> float *> float *> int *> eol

-- Grouping
groupName = kw "g" *> many1 token *> eol
smoothingGroup = kw "s" *> int *> eol
mergingGroup = kw "mg" *> int *> ((kw "off" *> return 0) <|> int) *> eol
objectName = kw "o" *> token *> eol

-- Display/render attributes
bevelInterpolation = kw "bevel" *> (kw "on" <|> kw "off") *> eol
colorInterpolation = kw "c_interp" *> (kw "on" <|> kw "off") *> eol
dissolveInterpolation = kw "d_interp" *> (kw "on" <|> kw "off") *> eol
levelOfDetail = kw "lod" *> int *> eol
materialName = kw "usemtl" *> token *> eol
materialLibrary = kw "mtllib" *> many1 token *> eol
shadowCasting = kw "shadow_obj" *> token *> eol
rayTracing = kw "trace_obj" *> token *> eol
curveApproximationTechnique = kw "ctech" *> (kw "cparm" *> float <|> kw "cspace" *> float <|> kw "curv" *> float *> float) *> eol
surfaceApproximationTechnique = "stech" *> (kw "cparma" *> float *> float <|> kw "cparmb" *> float <|> kw "cspace" *> float <|> kw "curv" *> float *> float) *> eol

-- MTL
--materialName = kw "newmtl" *> token *> eol

parseWavefrontOBJ = many statements *> mySkipSpace >> endOfInput
  where
    statements = vertexData <|> elements <|> freeFormCurves <|> connect <|> grouping <|> displayAttributes <|> comment

    vertexData = geometricVertice <|> textureVertice <|> vertexNormal <|> parameterSpaceVertice <|> curveOrSurfaceType <|> degree <|> basisMatrix <|> stepSize
    elements = point <|> line <|> face <|> curve <|> curve2D <|> surface
    freeFormCurves = parameterValues <|> outerTrimmingLoop <|> innerTrimmingLoop <|> specialCurve <|> specialPoint <|> endStatement
    grouping = groupName <|> smoothingGroup <|> mergingGroup <|> objectName
    displayAttributes = bevelInterpolation <|> colorInterpolation <|> dissolveInterpolation <|> levelOfDetail <|> materialName <|> materialLibrary <|> shadowCasting <|>
      rayTracing <|> curveApproximationTechnique <|> surfaceApproximationTechnique


main = do
  str <- B.getContents
  print $ parseOnly parseWavefrontOBJ str
