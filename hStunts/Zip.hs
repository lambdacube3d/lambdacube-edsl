module Zip where

{-
 Zip specification:
    http://en.wikipedia.org/wiki/Zip_(file_format)
    http://www.pkware.com/documents/casestudies/APPNOTE.TXT
-}

import Control.Applicative
import Data.Binary.Get
import Data.Bits
import Data.Word
import System.IO.MMap
import qualified Codec.Compression.Zlib.Raw as Zlib
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB

data Entry
    = Entry
    { eFilePath     :: !String
    , eIsCompressed :: !Bool
    , eData         :: !LB.ByteString
    }

type Archive = [Entry]

decompress' :: Entry -> LB.ByteString
decompress' (Entry _ False dat) = dat
decompress' (Entry _ True dat) = Zlib.decompress dat

decompress :: Entry -> SB.ByteString
decompress = SB.concat . LB.toChunks . decompress'

readArchive :: String -> IO Archive
readArchive n = runGet getArchive . LB.fromChunks . (:[]) <$> mmapFileByteString n Nothing

chunks :: Word32 -> Get a -> Get [a]
chunks c a = lookAhead getWord32le >>= \code -> case code == c of
    True    -> (:) <$> a <*> chunks c a
    False   -> return $! []

getArchive :: Get Archive
getArchive = chunks 0x04034b50 $ do
    -- local file header
    skip 6
    flag <- getWord16le
    isComp <- getWord16le >>= \i -> case i of
        0 -> return False
        8 -> return True
        _ -> fail "Unsupported compression method!"
    skip 8
    size <- getWord32le
    skip 4
    nameLen <- getWord16le
    extraLen <- getWord16le
    name <- SB.unpack <$> getByteString (fromIntegral nameLen)
    skip $! fromIntegral extraLen
    d <- if flag .&. 8 /= 0 then fail "Zip data descriptor is not supported!" else getLazyByteString $! fromIntegral size
    return $! Entry name isComp d
