{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Stunts.Unpack
    ( unpackResource
    , unpackResource'
    , loadResource
    ) where

import Control.Applicative
import Control.Arrow
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

splits :: [a] -> [Int] -> ([a], [[a]])
splits = mapAccumL $ \as i -> let (bs,cs) = splitAt i as in (cs, bs)

unpackVLE :: [Int] -> [Int]
unpackVLE (widthsCount: xs) = unfoldr (readDict dict) bitstream
  where
    widthsLen = widthsCount .&. 0x7f
    (widths,xs') = splitAt widthsLen xs
    (xs'', alphabet) = splits xs' widths
    bitstream = [testBit x n | x <- xs'', n <- [7,6..0]]
    dict = foldl go Empty $ zip [1..] $ snd $ annotate (0 :: Int) alphabet
      where
        annotate = mapAccumL $ (((`shiftL` 1) *** id) .) . mapAccumL (\n a -> (n+1, (n,a)))

        go d (l, as) = foldl go' d as
          where
            go' d (n,a) = insert d $ map (testBit n) [l-1,l-2.. -1]
              where
                insert (d1 :|: d2) (b:bs) = if b then d1 :|: insert d2 bs
                                                 else insert d1 bs :|: d2
                insert Empty (_:bs) = foldl (\x _ -> x :|: Empty) (Leaf a) bs

    readDict (d1 :|: d2) (b:bs) = readDict (if b then d2 else d1) bs
    readDict (Leaf x)    bs     = Just (x,bs)
    readDict _           _      = Nothing
