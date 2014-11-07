{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Parse
    ( getLabels
    ) where

import Numeric
import Data.Char
import Data.Bits
import Data.List
import Data.Function
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Control.Applicative

import Hdis86
import Hdis86.Incremental

data Info
    = OP BS.ByteString
    | OPErr String
    | Bytes Int
    | Label BS.ByteString
        deriving (Eq, Ord, Show)

showInfo (x, i) = case x of
    OP _ -> "_" :: String
    Bytes _ -> " "
    OPErr x -> x
    Label _ -> "."

showInfo' (x, i) = case x of
    Label x -> [(i, x)]
    _ -> []

showHex' i x = replicate (i - length s) '0' ++ s where s = showHex x ""

hd = BSC.pack . head . words . mdAssembly

diag = [(j, i-j) | i <- [1..], j<-[0..i]]

try xs ys = [(a,b) | (a,b)<- take 20 $ diag, drop a xs ~= drop b ys]

xs ~= ys = take 3 xs == take 3 ys

opss xs = [x | OP x <- xs]

drop' 0 xs = ([], xs)
drop' n [] = ([], [])
drop' n (x@(_, OP _): xs) = (x:a, b) where (a,b) = drop' (n-1) xs
drop' n (x:xs) = (x:a, b) where (a,b) = drop' n xs

align :: Int -> Int -> BS.ByteString -> [(BS.ByteString, Info)] -> [(Info, Int)]
align _ i s [] = []
--align i s (OP "nop": xs) = align
align er i s st@((orig, OP x): xs)
    | xx == x = (OP xx, i): align er (i + fromIntegral (mdLength md)) s' xs
    | otherwise = case try (opss $ map snd st) (map hd st') of
        ((a,b):_) -> (OPErr (showHex' 5 i ++ ": " ++ intercalate "; " (map (BSC.unpack . fst) a') ++ " / " ++ intercalate "; " (map mdAssembly $ disassembleMetadata disasmConfig $ BS.take lenn s)), i): align er (i + lenn) (BS.drop lenn s) st''
          where
            (a', st'') = drop' a st
            lenn = fromIntegral $ sum $ map (mdLength) $ take b st'
        _ -> error $ "align: " ++ show (take 10 st) ++ "   " ++ show (take 10 $ map hd st')
  where
    st' = disassembleMetadata disasmConfig s
    Just (md, s') = disassembleOne disasmConfig s
    xx = BSC.pack (head $ words $ mdAssembly md)
    len = fromIntegral $ mdLength md
align er i s ((orig, x@(Bytes j)): xs) = (x, i): align er (i+j) (BS.drop j s) xs
align er i s ((orig, x@(Label l)): xs) = (x, i): align (min 5 $ 1 + er) i s xs

norm "repe" = "rep"
norm "jnb" = "jae"
norm "iret" = "iretw"
norm "retn" = "ret"
norm "pushf" = "pushfw"
norm "xlat" = "xlatb"
norm a = a

opp "nop" = Bytes 1
opp s = OP $ norm s

disasmConfig = Config Intel Mode16 SyntaxIntel 0

getLabels_ = do
    s <- mapM BS.readFile [ "../restunts/src/restunts/asm/" ++ x
                          | x <- ("structs.inc":) $ map (++ ".asm") $ (++["dseg"]) $ map (("seg0" ++) . reverse . take 2 . (++ "0") . reverse . show) ([0..39] ++ [41])]
    ss <- loadExe <$> BS.readFile "../original/game.exe"
    let lines = map remo $ concatMap BSC.lines s
    return $ align 5 0 ss $ getInfo $ map (\x -> (x, BS.length x /= 0 && BS.index x 0 == 0x20, BSC.words x)) lines

getLabels :: IO (IM.IntMap BS.ByteString)
getLabels = IM.fromListWith comb . concatMap showInfo' <$> getLabels_

getLabels' = concatMap showInfo <$> getLabels_

comb a b = BS.concat [a, "; ", b]

ord' = fromIntegral . ord

getInfo :: [(BS.ByteString, Bool, [BS.ByteString])] -> [(BS.ByteString, Info)]
getInfo [] = []
getInfo ((orig, tab, s): bs)
  | not tab = case s of
--    [] ->
    (i:[]) | BS.last i == ord' ':' -> emp (Label i): cont
    (l:"db":x) -> emp (Label l): g orig "db" x ++ cont
    (l:"dw":x) -> emp (Label l): g orig "dw" x ++ cont
    (l:"dd":x) -> emp (Label l): g orig "dd" x ++ cont
    (s:"proc":"far":[]) -> emp (Label s): cont
    (s:"proc":"near":[]) -> emp (Label s): cont
    (s:"endp":[]) -> cont
    (s:"struc":[]) -> getInfo $ tail $ dropWhile ((/= (False, [s,"ends"])) . ff) bs
    (s:"union":[]) -> getInfo $ tail $ dropWhile ((/= (False, [s,"ends"])) . ff) bs
    (".model":_) -> cont
    ("smart":_) -> cont
    ("nosmart":_) -> cont
    (_:"=":_) -> cont
    ("end":_) -> cont
    (s:"group":_) -> cont
    (s:"segment":_) -> cont
    (s:"ends":[]) -> cont
    (".stack":n:[]) -> (orig, Bytes $ read $ BSC.unpack n): cont
    [] -> cont
    x -> error $ show x
  | otherwise = case s of
    (op:_) | op `S.member` opset -> (orig, opp op): cont
    ("db":x) -> g orig "db" x ++ cont
    ("dw":x) -> g orig "dw" x ++ cont
    ("dd":x) -> g orig "dd" x ++ cont
--    (x:_) | BS.head x == ord' ';' -> ""
    (_:"=":_) -> cont
    ("public":_) -> cont
    ("assume":_) -> cont
    ("include":_) -> cont
    [] -> cont
    x -> error $ show (" ":x)
  where
    ff (a,b,c) = (b,c)
    cont = getInfo bs
    emp x = (orig, x)

opset :: S.Set BS.ByteString
opset = S.fromList ["nop", "push", "pushf", "pusha", "pop", "popa", "mov", "movsw", "movsb", "lodsb", "lodsw", "stosb", "stosw", "scasb", "scasw", "cmpsb", "cmpsw", "xlat", "lahf", "sahf", "xchg", "or", "and", "xor", "neg", "not", "add", "adc", "sub", "sbb", "idiv", "div", "imul", "mul", "inc", "dec", "cmp", "test", "jcxz", "ja", "jna", "jb", "jnb", "jbe", "jnbe", "jl", "jle", "jnz", "jz", "jge", "jg", "js", "jo", "jno", "jns", "jmp", "cwd", "cbw", "call", "ret", "retf", "retn", "shl", "shr", "rcl", "rcr", "rol", "ror", "sal", "sar", "les", "lds", "lea", "repne", "rep", "repe", "loop", "int", "iret", "cli", "sti", "clc", "stc", "cld", "std", "out", "in"]

g orig c x = case x of
    (i:[]) | BSC.all isDigit i || isLabel i -> b
    ("offset": l: []) | isLabel l -> b
    ("seg": l: []) | isLabel l -> b
    ("?": []) -> b
    x -> error $ c ++ " " ++ show x
  where
    b = [(orig, Bytes $ case c of
        "db" -> 1
        "dw" -> 2
        "dd" -> 4)]

isLabel x = BSC.all (\c -> isAlphaNum c || c `elem` "_.+-") x

remo = BSC.takeWhile (/= ';')


loadExe :: BS.ByteString -> BS.ByteString
loadExe gameExe = BS.drop headerSize gameExe
  where
    paragraphsInHeader = ba 8 + ba 9 `shiftL` 8
    headerSize = paragraphsInHeader `shiftL` 4

    ba i = fromIntegral $ BS.index gameExe i


