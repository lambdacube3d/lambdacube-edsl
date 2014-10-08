{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Stunts.Unpack
    ( unpackResource
    , unpackResource'
    , loadResource
    ) where

import Control.Applicative
import Data.Bits
import Data.List
import Data.Vector ((!), (//))
import System.IO
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V

data Dict = Empty | Leaf Int | Dict :|: Dict

loadResource :: FilePath -> IO [Int]
loadResource path = withBinaryFile path ReadMode $ \hdl ->
    force =<< unpack . map fromEnum <$> hGetContents hdl
  where
    force xs = last xs `seq` return xs

unpackResource :: LB.ByteString -> LB.ByteString
unpackResource = LB.pack . map fromIntegral . unpack . map fromIntegral . LB.unpack

unpackResource' :: SB.ByteString -> SB.ByteString
unpackResource' = SB.pack . map fromIntegral . unpack . map fromIntegral . SB.unpack

unpack :: [Int] -> [Int]
unpack (n:xs)
    | testBit n 7 = iterate pass (drop 3 xs) !! (n .&. 0x7f)
    | otherwise   = pass (n:xs)
  where
    pass (compType:b1:b2:b3:xs) = take l $ case compType of
        1 -> unpackRLE (drop 4 xs)
        2 -> unpackVLE xs
        _ -> error "Unknown compression type!"
      where
        l = b1 + b2 `shiftL` 8 + b3 `shiftL` 16

unpackRLE :: [Int] -> [Int]
unpackRLE (escCount:xs) = decodeBytes $
                          if escCount > 0x7f then rest else decodeSeq (escCodes !! 1) rest
  where
    (escCodes,rest) = splitAt (escCount .&. 0x7f) xs
    escLookup = V.replicate 0x100 0 // zip escCodes [1..]

    decodeSeq code = concat . go
      where
        go [] = []
        go (c:cs)
            | c == code = replicate rep run ++ go rest
            | otherwise = [c] : go cs
          where
            (run,_:rep:rest) = break (==code) cs

    decodeBytes [] = []
    decodeBytes (c:cs) = case escLookup ! c of
        0 -> c : decodeBytes cs
        1 -> let n:c:cs' = cs in replicate n c ++ decodeBytes cs'
        3 -> let n1:n2:c:cs' = cs in replicate (n1 + n2 `shiftL` 8) c ++ decodeBytes cs'
        n -> let c:cs' = cs in replicate (n-1) c ++ decodeBytes cs'

unpackVLE :: [Int] -> [Int]
unpackVLE (widthsCount:xs) = unfoldr (readDict dict) stream
  where
    widthsLen = widthsCount .&. 0x7f
    (widths,xs') = splitAt widthsLen xs
    (alphabet,xs'') = splitAt (sum widths) xs'
    stream = [testBit x n | x <- xs'', n <- [7,6..0]]
    dict = buildDict widths alphabet

    buildDict ws as = go ws as 1 (0 :: Int) Empty
      where
        go []     _  _ _ d = d
        go (w:ws) as l n d
            | w == 0    = go ws as (l+1) (n `shiftL` 1) d
            | otherwise = go (w-1:ws) as' l (n+1) (add l d a)
          where
            a:as' = as
            add 0  _     a = Leaf a
            add l' Empty a = add (l'-1) Empty a :|: Empty
            add l' (d1 :|: d2) a
                | testBit n (l'-1) = d1 :|: add (l'-1) d2 a
                | otherwise        = add (l'-1) d1 a :|: d2

    readDict (Leaf x)    bs     = Just (x,bs)
    readDict (d1 :|: d2) (b:bs) = readDict (if b then d2 else d1) bs
    readDict _           _      = Nothing
