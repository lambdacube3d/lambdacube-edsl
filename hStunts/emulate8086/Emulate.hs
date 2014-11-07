{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Emulate where

import Numeric
import Data.Word
import Data.Int
import Data.Bits hiding (bit)
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Error
import Control.Lens as Lens
import Control.Concurrent.MVar
import System.Directory
import System.FilePath (takeFileName)
import System.FilePath.Glob

import System.IO.Unsafe

import Hdis86
import Hdis86.Incremental

----------------------------------------------

everyNth n [] = []
everyNth n xs = take n xs: everyNth n (drop n xs)

seqToList :: S.Seq a -> [a]
seqToList s = case S.viewl s of
    S.EmptyL -> []
    x S.:< xs -> x: seqToList xs

take' _ [] = []
take' 0 _ = ["..."]
take' n (x:xs) = x: take' (n-1) xs

showHex' :: (Show a, Integral a) => Int -> a -> [Char]
showHex' i x = replicate (i - length s) '0' ++ s where s = showHex x ""

pad :: Int -> String -> String
pad i s = s ++ take (i - length s) (repeat ' ')

align :: Integral a => a -> a -> a
align n i = ((i + n - 1) `div` n) * n

assert _ True = return ()
assert m _ = error m

quotRemSafe :: Integral a => a -> a -> Maybe (a, a)
quotRemSafe a 0 = Nothing
quotRemSafe a b = Just $ quotRem a b


----------------------------------------------

uComb :: Lens' a b -> Lens' a c -> Lens' a (b, c)
uComb x y = lens ((^. x) &&& (^. y)) $ \a (b, c) -> set x b . set y c $ a

(>-) :: Getting a s a -> (a -> b -> s -> t) -> b -> s -> t
k >- f = too $ f . (^. k)

too f tr a = f a tr a

---------------------------------

class (Integral a, FiniteBits a, Integral (Signed a), FiniteBits (Signed a)) => AsSigned a where
    type Signed a :: *
    asSigned :: a -> Signed a

instance AsSigned Word8 where
    type Signed Word8 = Int8
    asSigned = fromIntegral
instance AsSigned Word16 where
    type Signed Word16 = Int16
    asSigned = fromIntegral
instance AsSigned Word32 where
    type Signed Word32 = Int32
    asSigned = fromIntegral

class (Integral a, Integral (X2 a)) => Extend a where
    type X2 a :: *
    extend :: a -> X2 a
    combine :: Iso' (a, a) (X2 a)

    high, low :: Extend a => Lens' (X2 a) a
    high = from combine . _1
    low  = from combine . _2

instance Extend Word8 where
    type X2 Word8 = Word16
    extend = fromIntegral
    combine = iso (\(hi,lo) -> fromIntegral hi `shiftL` 8 .|. fromIntegral lo) (\d -> (fromIntegral $ d `shiftR` 8, fromIntegral d))
instance Extend Word16 where
    type X2 Word16 = Word32
    extend = fromIntegral
    combine = iso (\(hi,lo) -> fromIntegral hi `shiftL` 16 .|. fromIntegral lo) (\d -> (fromIntegral $ d `shiftR` 16, fromIntegral d))
instance Extend Int8 where
    type X2 Int8 = Int16
    extend = fromIntegral
    combine = iso (\(hi,lo) -> fromIntegral hi `shiftL` 8 .|. fromIntegral lo) (\d -> (fromIntegral $ d `shiftR` 8, fromIntegral d))
instance Extend Int16 where
    type X2 Int16 = Int32
    extend = fromIntegral
    combine = iso (\(hi,lo) -> fromIntegral hi `shiftL` 16 .|. fromIntegral lo) (\d -> (fromIntegral $ d `shiftR` 16, fromIntegral d))

bit :: Bits a => Int -> Lens' a Bool
bit i = lens (`testBit` i) $ \x b -> if b then x `setBit` i else x `clearBit` i

highBit :: forall a . FiniteBits a => Lens' a Bool
highBit = bit $ finiteBitSize (undefined :: a) - 1

paragraph :: Iso' Word16 Int
paragraph = iso ((`shiftL` 4) . fromIntegral) (fromIntegral . (`shiftR` 4))

segAddr :: Word16 -> Word16 -> Int
segAddr s w = s ^. paragraph + fromIntegral w

----------------

data Annot
    = NoAnnot
    | NotInit
    | Low Annot
    | High Annot
    | CombAnn Annot Annot
    | Annot BS.ByteString
    deriving (Eq, Ord, Show)

data Ann a = Ann
    { _annot :: Annot
    , _ann :: a
    }

$(makeLenses ''Ann)

showAnn NoAnnot = ""
showAnn NotInit = "undef"
showAnn (Annot b) = BSC.unpack b
showAnn (CombAnn a b) = showAnn a ++ ":" ++ showAnn b
showAnn (Low a) = "lo " ++ showAnn a
showAnn (High a) = "hi " ++ showAnn a

annMap x f (Ann a b) = Ann (g a) (f b) where
    g (Annot i) = Annot $ x `BS.append` i
    g _ = NoAnnot

combineAnnot :: Iso' (Ann a, Ann a) (Ann (a, a))
combineAnnot = iso f g
  where
    f (Ann a x, Ann b y) = Ann (combAnn a b) (x, y)
    g (Ann a (x, y)) = (Ann (low a) x, Ann (high a) y)

    low NoAnnot = NoAnnot
    low NotInit = NotInit
    low (CombAnn a b) = a
    low a = Low a
    high NoAnnot = NoAnnot
    high NotInit = NotInit
    high (CombAnn a b) = b
    high a = High a
    combAnn NotInit _ = NotInit
    combAnn _ NotInit = NotInit
    combAnn NoAnnot NoAnnot = NoAnnot
    combAnn (Low a) (High a') | a == a' = a
    combAnn a b = CombAnn a b

clearAnn = iso (^. ann) $ Ann NoAnnot
notInit x = Ann NotInit $ error $ "not initialized " ++ x

noAnn = Ann NoAnnot
b @: x = Ann (Annot b) x
infix 5 @:

instance (Eq a) => Eq (Ann a) where
    Ann NotInit _ == _ = False
    _ == Ann NotInit _ = False
    a == b = a ^. ann == b ^. ann

instance Show a => Show (Ann a) where
    show (Ann NotInit a) = "undef2"
    show (Ann x a) = show x ++ " @: " ++ show a

instance Functor Ann where
    fmap f (Ann x y) = Ann x (f y)

showA (Ann NotInit a) = "undef3"
showA (Ann NoAnnot a) = a
showA (Ann x a) = a ++ "{" ++ showAnn x ++ "}"


class Rom a where
    readByte_ :: a -> Int -> Maybe (Ann Word8)
    readByte :: a -> Int -> Ann Word8
    readByte a i = fromMaybe (error $ "read non initialized memory at " ++ showHex' 5 i) $ readByte_ a i
    toRom :: [Word8] -> a
    fromRom :: Int -> Int -> a -> [Word8]
    fromRom i j v = [readByte v k ^. ann | k<-[i..i+j-1]]


class Rom a => Ram a where
    writeByte_ :: Int -> Ann Word8 -> a -> Maybe a
    writeByte_ i x s = const (writeByte i x s) <$> readByte_ s i
    writeByte :: Int -> Ann Word8 -> a -> a
    writeByte i x a = fromMaybe (error $ "write non initialized memory at " ++ showHex' 5 i) $ writeByte_ i x a

    byteAt' :: Int -> Lens' a (Ann Word8)
    byteAt' i = lens (`readByte` i) $ \x b -> writeByte i b x

class Rom a => ExtRom a where
    extendRom :: Word16 -> a -> (Word16, a)

instance Rom (V.Vector (Ann Word8)) where
    toRom = V.fromList . map noAnn
    fromRom i j = map (^. ann) . take j . V.toList . V.drop i
    -- sLength = V.length
    -- sJoin = (V.++)
    readByte_ s i | 0 <= i && i < V.length s = Just $ (s V.! i)
                  | otherwise = Nothing

-- non-rigid!
instance Rom (IM.IntMap (Ann Word8)) where
    toRom = IM.fromList . zip [0..] . map noAnn
    readByte_ s i = IM.lookup i s

instance Ram (IM.IntMap (Ann Word8)) where
    writeByte_ i x = Just . IM.insert i (x)

data Overlay a b = Overlay a b

data ERom
    = Rom (V.Vector (Ann Word8))
    | Split Int ERom ERom
    | Zeros Bool Int Int

showMem (Overlay a b) = "Overlay _ (" ++  showRom b ++ ")"

showRom :: ERom -> String
showRom (Rom v) = "Rom " ++ showHex' 5 (V.length v)
showRom (Split i a b) = "Split " ++ showHex' 5 i ++ " (" ++ showRom a ++ ") (" ++ showRom b ++ ")"
showRom (Zeros _ i j) = "Zeros " ++ showHex' 5 i ++ " " ++ showHex' 5 j

extendRom' :: Int -> Int -> ERom -> ERom
extendRom' beg size r@(Rom v)
    | beg + size <= V.length v = r
    | beg <= V.length v = Split (V.length v) r $ Zeros False (V.length v) $ beg + size - V.length v
extendRom' beg size (Split i a b)
    | beg + size <= i = Split i (extendRom' beg size a) b
    | beg >= i = Split i a (extendRom' beg size b)
--    | beg <= V.length v = Split (V.length v) r $ Zeros (V.length v) $ beg + size - 1
--extendRom' beg size (Zeros a b)
extendRom' beg size (Zeros False a b)
    | a <= beg && beg <= a + b = Zeros False a $ max b $ beg - a + size
extendRom' a b r = error $ "extendRom' " ++ showHex' 5 a ++ " " ++ showHex' 5 b ++ " (" ++ showRom r ++ ")"


instance Rom ERom where
    readByte_ (Rom a) i = readByte_ a i
    readByte_ (Zeros True a b) i
        | a <= i && i < a + b = Just ("mb" @: 0xfa) -- $ error "mem uninitialized byte 2") --noAnn 0)
    readByte_ (Zeros False a b) i
        | a <= i && i < a + b = Just (notInit $ "byte 2") --noAnn 0)
--        | a <= i && i < a + 0x10 = Just (noAnn $ error "mem uninitialized byte") --noAnn 0)
--        | a + 0x10 <= i && i < a + b = Just ("mb" @: 0xfa) -- $ error "mem uninitialized byte 2") --noAnn 0)
        | otherwise        = Nothing
    readByte_ (Split v a b) i
        | i < v = readByte_ a i
        | otherwise        = readByte_ b i
    toRom = Rom . toRom

instance (Rom a, Rom b) => Rom (Overlay a b) where
    readByte_ (Overlay s v) i = maybe (readByte_ v i) Just $ readByte_ s i
--    writeByte_ i b (Overlay s v) = maybe (Overlay s <$> writeByte_ i b v) (Just . (`Overlay` v)) $ writeByte_ i b s
    toRom l = Overlay (toRom []) (toRom l)
instance (Ram a, Rom b) => Ram (Overlay a b) where
    writeByte_ i b (Overlay s v)
        = const (Overlay (writeByte i b s) v) <$> readByte_ v i

instance ExtRom ERom where
    extendRom x v = 
        (len ^. from paragraph, w)
      where
        needed = x ^. paragraph
        (len, w) = head $ f 0x100000 v
        f u (Split v a b) = [(l, Split v a' b) | (l, a') <- f v a] ++ [(l, Split v a b') | (l, b') <- f u b]
        f u w@(Zeros False a b) = [add x w | let x = align' (a + b), x + needed <= u ]
        f u w@(Rom v) = [add x w | let x = align' $ V.length v, x + needed <= u]
        align' = align 16

        add len v = (len, Split len v (Zeros False len $ x ^. paragraph))

instance (Ram a, ExtRom b) => ExtRom (Overlay a b) where
    extendRom x (Overlay s v) = (r, Overlay s y)
      where
        (r, y) = extendRom x v

extendMem :: (ExtRom s, MonadState s m) => Word16 -> m Word16
extendMem x = state $ extendRom x

byteAt :: Ram a => Int -> Lens' a Word8
byteAt i = byteAt' i . clearAnn

bytesAt :: Ram a => Int -> Int -> Lens' a [Word8]
bytesAt i j = lens (fromRom i j) $ \x bs -> foldl (flip ($)) x $ zipWith writeByte [i..i+j-1] $ map noAnn bs

wordAt' :: Ram a => Int -> Lens' a (Ann Word16)
wordAt' i = (byteAt' (i+1) `uComb` byteAt' i) . combineAnnot . mapping combine

wordAt :: Ram a => Int -> Lens' a Word16
wordAt i = (byteAt (i+1) `uComb` byteAt i) . combine

dwordAt' :: Ram a => Int -> Lens' a (Ann Word32)
dwordAt' i = (wordAt' (i+2) `uComb` wordAt' i) . combineAnnot . mapping combine

dwordAt :: Ram a => Int -> Lens' a Word32
dwordAt i = (wordAt (i+2) `uComb` wordAt i) . combine

----------------

-- size in bytes
type Size = Int

data Key
    = KReg Size Int
    | Flag Int
    | Flags
    | Heap Size Int
        deriving (Eq, Ord)

flagnames = "____oditsz_a_p_c"
reg8names = ["al","ah","dl","dh","bl","bh","cl","ch"]
reg16names = ["ax","dx","bx","cx", "si","di", "cs","ss","ds","es", "ip","sp","bp"]

combineKey (Heap s i: ks) | s `elem` [1,2] = run (i + s) ks
  where
    run i (Heap s' i': ks) | i==i' && s==s' = run (i + s) ks
    run i' ks = Heap (i'-i) i: combineKey ks
combineKey (k:ks) = k: combineKey ks
combineKey [] = []

instance Show Key where
    show (Heap _ i) = "[" ++ showHex' 5 i ++ "]"
    show (Flag i) = [flagnames !! (15-i)]
    show Flags = "flags"
    show (KReg 1 i) = reg8names !! i
    show (KReg 2 i) = reg16names !! (i `div` 2)
    show (KReg 4 0) = "ax:dx"

data Config_ = Config_
    { _numOfDisasmLines :: Int
    , _disassStart :: Int
    , _verboseLevel :: Int
    , _termLength :: Int
    , _videoMVar :: MVar (Int -> Int -> Word8)
    , _instPerSec :: Int

    , _stackTrace :: [Int]
    , _stepsCounter :: Int

    , _counter :: Maybe Int -- counter to count down
    , _speaker :: Word8     -- 0x61 port
    }

$(makeLenses ''Config_)

defConfig = Config_
    { _numOfDisasmLines = 30
    , _disassStart = 0
    , _verboseLevel = 2
    , _termLength = 149
    , _instPerSec = 1000000
    , _videoMVar = undefined --return $ \_ _ -> 0

    , _stackTrace = []
    , _stepsCounter = 0

    , _counter = Nothing
    , _speaker = 0  -- speaker off
    }

data MachineState = MachineState
    { _flags'   :: Ann Word16
    , _regs     :: IM.IntMap (Ann Word8)
    , _heap     :: Overlay (IM.IntMap (Ann Word8)) ERom

    , _hist     :: Set.Set Key
    , _traceQ   :: [String]
    , _config   :: Config_
    , _cache    :: IM.IntMap (Machine ())
    , _labels   :: IM.IntMap BS.ByteString
    , _files    :: IM.IntMap (FilePath, BS.ByteString, Int)  -- filename, file, position
    , _dta      :: Int
    , _retrace  :: [Word16]
    , _intMask  :: Word8
    }

emptyState = MachineState
    { _flags'   = "flags" @: 0x7202
    , _regs     = toRom (replicate 26 0)
    , _heap     = toRom []

    , _hist     = Set.empty
    , _traceQ   = []
    , _config   = defConfig
    , _cache    = IM.empty
    , _labels   = IM.empty
    , _files    = IM.empty
    , _dta      = 0
    , _retrace  = cycle [1,9,0,8]
    , _intMask  = 0xff
    }

data Halt
    = Halt
    | Err String
  deriving Show

instance Error Halt where

type Machine = ErrorT Halt (State MachineState)

$(makeLenses ''MachineState)

setCounter = do
    trace_ "setCounter"
    v <- use $ config . instPerSec
    config . counter .= Just (v `div` 24)

getRetrace = do
    retrace %= tail
    head <$> use retrace


trace_ :: String -> Machine ()
trace_ s = traceQ %= (s:)

steps = config . numOfDisasmLines

clearHist = do
    hi <- use hist
    hist .= Set.empty
    h <- use traceQ
    traceQ .= []
    return (hi, intercalate "; " $ reverse h)

type MachinePart a = Lens' MachineState a

addHist :: Key -> MachinePart a -> MachinePart a
addHist sh k = lens (^. k) $ \s a -> hist %~ Set.insert sh $ set k a s

flagLenses@[_,_,_,_,overflowF,directionF,interruptF,trapF,signF,zeroF,_,adjustF,_,parityF,_,carryF]
    = [ addHist (Flag i) $ flags' . ann . bit i | i <- [15,14..0] ]
reg16Lenses@[ax,dx,bx,cx, si,di, cs,ss,ds,es, ip,sp,bp]
    = [ addHist (KReg 2 i) $ regs . wordAt i | i <- [0,2..24] ]
reg8Lenses@[al,ah,dl,dh,bl,bh,cl,ch]
    = [ addHist (KReg 1 i) $ regs . byteAt i | i <- [0..7] ]
dxax = addHist (KReg 4 0) $ regs . dwordAt' 0


segAddr_ seg off = to $ \s -> segAddr (s ^. seg) (s ^. off)

ips = segAddr_ cs ip
sps = segAddr_ ss sp

xx :: MachinePart (Ann Word16)
xx = lens (const $ error "xx") $ \s _ -> s

flags = flags'' . ann
flags'' = addHist Flags flags'

heap8  i = addHist (Heap 1 i) $ heap . byteAt' i
heap16 i = addHist (Heap 2 i) $ heap . wordAt' i

stackTop :: MachinePart (Ann Word16)
stackTop = sps >- heap16

-- experimental
reg16Lenses'@[ax',dx',bx',cx', si',di', cs',ss',ds',es', ip',sp',bp']
    = [ addHist (KReg 2 i) $ regs . wordAt' i | i <- [0,2..24] ]
[al',ah',dl',dh',bl',bh',cl',ch']
    = [ addHist (KReg 1 i) $ regs . byteAt' i | i <- [0..7] ]

----------------------

push :: Ann Word16 -> Machine ()
push x = do
    sp %= (+ (-2))
    stackTop .= x

pop :: Machine (Ann Word16)
pop = do
    x <- use stackTop
    sp %= (+ 2)
    return x

showFlags s = zipWith showFlag flagnames [s ^. bit i | i <- [15,14..0]]
  where
    showFlag '_' k = if k then '1' else '0'
    showFlag c k = if k then toUpper c else c

instance Show MachineState where
    show s = intercalate "\n" $
        [ "  Flags: " ++ showFlags (s ^. flags)
        , ("  "++) $ unwords $ zipWith showReg ["AX","BX","CX","DX"] [ax,bx,cx,dx]
        , ("  "++) $ unwords $ zipWith showReg ["SI","DI","IP","SP","BP"] [si,di,ip,sp,bp]
        , ("  "++) $ unwords $ zipWith showReg ["DS","ES","CS","SS"] [ds,es,cs,ss]

        , (\(a,b)->a ++ "\n" ++ b) $ (("Stack: " ++) . unwords) *** (("       " ++) . unwords) $ unzip
            $ take 20
            $ zip (map (showHex' 4) [s ^. sp, s ^. sp + 2..0xffff] ++ repeat "####")
                    (take 20 . map ff . everyNth 2 $ map (maybe "##" (showHex' 2. (^. ann)) . (readByte_ heap_)) [s ^. sps ..])
        , "Code: "
        ] ++ map (take $ s ^. config . termLength) (take' (s ^. config . numOfDisasmLines) $ showCode (initQueue s) s)
      where
        ff [a,b] = b ++ a
        heap_ = s ^. heap
        showReg c k = c ++ ":" ++ showHex' 4 (s ^. k)

infixr 5 .++, +.+

"" .++ b = b
a .++ b = a ++ " " ++ b

b +.+ "" = b
"" +.+ b = b
a +.+ b = a ++ "+" ++ b

nonSeg = \case
    Seg _ -> False
    x -> True

showInst s Metadata{mdLength = len, mdInst = Inst{..}} = showPrefix (filter nonSeg inPrefixes)
        .++ showOpcode inOpcode ++ " " ++ intercalate ", " (map showOp inOperands)
  where
    showOpcode op = tail $ show op

    segOverride = case [s | Seg s <- inPrefixes] of
        [] -> ""
        [s] -> showSeg s ++ ":"

    showPrefix = \case
        [Rep, RepE]
            | inOpcode `elem` [Icmpsb, Icmpsw, Iscasb, Iscasw] -> "repe"
            | otherwise -> "rep"
        [RepNE] -> "repne"
        [] -> ""

    showSeg = \case
        ES -> val16 es' "es"
        DS -> val16 ds' "ds"
        SS -> val16 ss' "ss"
        CS -> val16 cs' "cs"

    val8 k n = n ++ "{" ++ showA (showHex' 2 <$> s ^. k) ++ "}"
    val16 k n = n ++ "{" ++ showA (showHex' 4 <$> s ^. k) ++ "}"

    showReg = \case
        Reg8 r L -> case r of
            RAX -> val8 al' "al"
            RBX -> val8 bl' "bl"
            RCX -> val8 cl' "cl"
            RDX -> val8 dl' "dl"
        Reg8 r H -> case r of
            RAX -> val8 ah' "ah"
            RBX -> val8 bh' "bh"
            RCX -> val8 ch' "ch"
            RDX -> val8 dh' "dh"
        Reg16 r -> case r of
            RBP -> val16 bp' "bp"
            RSP -> val16 sp' "sp"
            RAX -> val16 ax' "ax"
            RBX -> val16 bx' "bx"
            RCX -> val16 cx' "cx"
            RDX -> val16 dx' "dx"
            RSI -> val16 si' "si"
            RDI -> val16 di' "di"
        RegSeg r -> showSeg r
        RegIP -> val16 ip' "ip"
        RegNone -> ""

    showSign v | v < 0 = "-"
               | otherwise = ""

    showOp x = case x of
        Reg r -> showReg r
        Imm (Immediate s v) -> case s of
            Bits8 -> showHex' 2 v ++ "h"
            Bits16 -> showHex' 4 v ++ "h"
        Jump (Immediate size v) -> showAdd (s ^. ips + fromIntegral len + fromIntegral v) $ case size of
            Bits8 -> showSign v ++ showHex' 2 (abs v) ++ "h"
            Bits16 -> showSign v ++ showHex' 4 (abs v) ++ "h"
        Hdis86.Const (Immediate Bits0 0) -> "1" -- !!! ?
        Ptr (Pointer seg (Immediate Bits16 off)) -> showHex' 4 seg ++ ":" ++ showHex' 4 (fromIntegral off)
        Mem (Memory s b i 0 off)
            -> "[" ++ segOverride ++ (showReg b +.+ showReg i +.+ showImm off) ++ "]"

    showAdd v i = maybe i BSC.unpack $ IM.lookup v (s ^. labels)

    showImm (Immediate s v) = case s of
        Bits0 -> ""
        Bits8 -> showHex' 2 (fromIntegral v :: Word8) ++ "h"
        Bits16 -> showHex' 4 (fromIntegral v :: Word16) ++ "h"

ifff "" = []
ifff x = [x]

type Queue = [MachineState]

initQueue :: MachineState -> Queue
initQueue s = [s]
addQueue :: MachineState -> Queue -> Queue
addQueue s q = length q' `seq` q'
  where
    q' = take 30 $ s: q
getQueueLast :: Queue -> MachineState
getQueueLast = last

--runTillHalt st = flip evalState st . runErrorT $ do

mkSteps :: MachineState -> (Halt, MachineState)
mkSteps s = either (\x -> (x, s')) (const $ either (\x -> (x, s')) (const $ mkSteps s') b) a
  where
    (a, b, s') = mkStep s

mkStep s = (x_, y, s') where
    (x__, s_) = flip runState s $ runErrorT $ do
        cc <- use $ config . counter
        if maybe False (<=0) cc
          then do
            trace_ "timer now"            
            config . counter .= Nothing
            --setCounter
            return $ Left $ Metadata {mdOffset = 0, mdLength = 2, mdHex = "cd08", mdBytes = "\205\b", mdAssembly = "int 0x8", mdInst = Inst [] Iint [Imm (Immediate {iSize = Bits8, iValue = 8})]}
          else do
            md <- fetchInstr
            _ <- clearHist
            config . counter %= fmap pred
            return $ Right md

    x_ = either id id <$> x__

    (y, s'_) = flip runState s_ $ runErrorT $ do        
        case x__ of
          Left _ -> timerInt
          Right _ -> do
            snd $ execInstruction $ either (error . show) id $ x_
        clearHist

    s' = s'_ & config . stepsCounter %~ succ & config . verboseLevel %~ max (if s'_ ^. config . stepsCounter > s'_ ^. config . disassStart then 2 else 1)

showCode :: Queue -> MachineState -> [String]
showCode q s = case x_ of
    Left e -> showErr e
    Right x -> case s ^. config . verboseLevel of 
      1 -> 
           ifff traces
        ++ [vid | ns `mod` ((s ^. config . instPerSec) `div` 25) == 0]
        ++ next
      2 -> 
        maybeToList (BSC.unpack <$> IM.lookup (s ^. ips) (s ^. labels))
         ++
        (("  " ++ pad 14 (map toUpper $ mdHex x)
         ++ " "++ pad 27 (showInst s x) 
         ++ "" ++ traces
         ++ (if inOpcode (mdInst x) `elem` [Icall] then " " ++ intercalate "/" (map BSC.unpack . catMaybes . map (`IM.lookup` (s ^. labels)) $ s ^. config . stackTrace) else "")
         ++ "  " ++ unwords (map shKey $ combineKey $ Set.toList hist_)): next
        )
 where
    ns = s ^. config . stepsCounter

    vid = unsafePerformIO $ do
        let gs = 0xa0000 --0x30000 -- x ^. heap16 0x6 . ann . paragraph
            v = s ^. heap
        putMVar (s ^. config . videoMVar) $ \x y -> maybe 0 (^. ann) $ readByte_ v (gs + 320 * y + x)
        return $ show ns

    (hist_, traces) = case y of
        Left e -> (Set.empty, "lost history")
        Right s -> s

    next = case y of
        Left e -> showErr e
        Right _ -> q' `seq` showCode q' s'
    q' = addQueue s' q

    showErr e = case s ^. config . verboseLevel of
      1 -> showCode (initQueue s'') s''
      _ -> show e: []
     where
        s'' = getQueueLast q & config . verboseLevel .~ 2

    (x_, y, s') = mkStep s

    shKey k = case k of
        Heap 1 i  -> diff (sh'' 1) $ heap . byteAt' i
        Heap 2 i  -> diff (sh'' 2) $ heap . wordAt' i
        Heap n i  -> "[" ++ showHex' 5 i ++ "-" ++ showHex' 5 (i+n-1) ++ "]"
        Flag i    -> diff sf $ flags . bit i
        Flags     -> diff showFlags flags
        KReg 1 i  -> diff (sh'' 1) $ regs . byteAt' i
        KReg 2 i  -> diff (sh'' 2) $ regs . wordAt' i
        KReg 4 i  -> diff (sh 4) $ regs . dwordAt i
      where
        diff :: Eq a => (a -> String) -> Lens' MachineState a -> String
        diff f l = par (v == v') $ f v'
          where
            v = s ^. l
            v' = s' ^. l

        lok i def = maybe def (\s -> "[" ++ BSC.unpack s ++ "]") $ IM.lookup i (s ^. labels)

        sh :: (Show a, Integral a) => Int -> a -> String
        sh i v = show k ++ ":" ++ showHex' (2*i) v

        sh'' :: (Show a, Integral a) => Int -> Ann a -> String
        sh'' i v = lok i (show k) ++ ":" ++ showA (showHex' (2*i) <$> v)

        sf True = map toUpper $ show k
        sf False = show k

        par True a = "(" ++ a ++ ")"
        par False a = a

sizeByte_ i@Inst{..}
    | inOpcode `elem` [Icbw, Icmpsb, Imovsb, Istosb, Ilodsb, Iscasb] = 1
    | inOpcode `elem` [Icwd, Icmpsw, Imovsw, Istosw, Ilodsw, Iscasw] = 2
    | inOpcode == Iout  = fromJust $ operandSize $ inOperands !! 1
    | otherwise = fromMaybe (error $ "size: " ++ show i) $ listToMaybe $ catMaybes $ map operandSize inOperands

operandSize = \case
    Reg (Reg16 _)   -> Just 2
    Reg (Reg8 _ _)  -> Just 1
    Mem (Memory Bits8 _ _ _ _)  -> Just 1
    Mem (Memory Bits16 _ _ _ _) -> Just 2
    Imm (Immediate Bits8 v)  -> Just 1
    Imm (Immediate Bits16 v) -> Just 2
    _ -> Nothing

segOf = \case
    RegIP     -> cs
    Reg16 RSP -> ss
    Reg16 RBP -> ss
    Reg16 RDI -> ds -- es?
    _         -> ds

reg = \case
    Reg16 r -> case r of
        RBP -> bp'
        RSP -> sp'
        RAX -> ax'
        RBX -> bx'
        RCX -> cx'
        RDX -> dx'
        RSI -> si'
        RDI -> di'
    RegSeg r -> case r of
        ES -> es'
        DS -> ds'
        SS -> ss'
        CS -> cs'
    RegIP -> ip'
    RegNone -> immLens $ noAnn 0
    x -> error $ "reg: " ++ show x

addressOf :: Maybe Segment -> Memory -> Getter MachineState Int
addressOf segmentPrefix m
    = segAddr_ (maybe (segOf $ mBase m) ((. ann) . reg . RegSeg) segmentPrefix) (addressOf' m)

addressOf' :: Memory -> Getter MachineState Word16
addressOf' (Memory _ r r' 0 i) = to $ \s -> imm i + s ^. (reg r . ann) + s ^. (reg r' . ann)
addressOf' m = error $ "addressOf: " ++ show m

byteOperand :: Maybe Segment -> Operand -> MachinePart (Ann Word8)
byteOperand segmentPrefix x = case x of
    Reg r -> case r of
        Reg8 r L -> case r of
            RAX -> al'
            RBX -> bl'
            RCX -> cl'
            RDX -> dl'
        Reg8 r H -> case r of
            RAX -> ah'
            RBX -> bh'
            RCX -> ch'
            RDX -> dh'
    Mem m -> addressOf segmentPrefix m >- heap8
    Imm (Immediate Bits8 v) -> immLens $ noAnn $ fromIntegral v
    Hdis86.Const (Immediate Bits0 0) -> immLens $ noAnn 1 -- !!!
    _ -> error $ "byteOperand: " ++ show x

wordOperand :: Maybe Segment -> Operand -> MachinePart (Ann Word16)
wordOperand segmentPrefix x = case x of
    Reg r -> reg r
    Mem m -> addressOf segmentPrefix m >- heap16
    Imm i -> immLens $ noAnn $ imm' i
    Jump i -> ip >- (immLens . noAnn . (+ imm i))
    _ -> error $ "fetch: " ++ show x

imm = fromIntegral . iValue
-- patched version
imm' (Immediate Bits8 i) = fromIntegral (fromIntegral i :: Int8)
imm' i = imm i

immLens c = lens (const c) $ \_ _ -> error "can't update immediate value"

memIndex r = Memory undefined (Reg16 r) RegNone 0 $ Immediate Bits0 0

fetchInstr :: Machine Metadata
fetchInstr = do
    cs_ <- use cs
    case cs_ of

      0xffff -> do
        ip_ <- use ip
        return $ Metadata {mdOffset = 0, mdLength = 0, mdHex = "cdxx", mdBytes = BS.pack [fromIntegral ip_], mdAssembly = "int", mdInst = Inst [] Iint [Imm (Immediate {iSize = Bits8, iValue = fromIntegral ip_})]}

      _ -> do
        ips <- use ips
        Just (md, _) <- disassembleOne disasmConfig . BS.pack . fromRom ips maxInstLength <$> use heap
        ip %= (+ fromIntegral (mdLength md))
        return md

maxInstLength = 7

disasmConfig = Config Intel Mode16 SyntaxIntel 0

cachedStep :: Machine ()
cachedStep = do
    ips <- use ips
    c <- use cache
    case IM.lookup ips c of
      Just m -> m
      _ -> do
        let collect = do
                md <- fetchInstr
                ip' <- use ip
                let (jump, m_) = execInstruction md
                    m = ip .= ip' >> m_
                m_
                (m >>) <$> if jump
                  then return (return ())
                  else collect

        m <- collect
        cache %= IM.insert ips m

execInstruction :: Metadata -> (Bool, Machine ())
execInstruction mdat@Metadata{mdInst = i@Inst{..}}
  | mdLength mdat == 0 = (True, origInterrupt $ head $ BS.unpack $ mdBytes mdat) 
  | otherwise = case filter nonSeg inPrefixes of
    [Rep, RepE]
        | inOpcode `elem` [Icmpsb, Icmpsw, Iscasb, Iscasw] -> (,) jump $ cycle $ use zeroF      -- repe
        | otherwise -> (,) jump cycle'      -- rep
    [RepNE] -> (,) jump $ cycle $ not <$> use zeroF
    [] -> (,) jump body
    x -> error $ "execInstruction: " ++ show x
  where
    (jump, body) = execInstructionBody $ mdat { mdInst = i { inPrefixes = filter (not . rep) inPrefixes }}

    cycle' = do
        c <- use cx
        replicateM_ (fromIntegral c) body
        cx .= 0
        -- zeroF .= True ?

    cycle cond = use cx >>= m >>= (cx .=)
      where
        m 0 = return 0
        m n = do
            body
            let n' = n-1
            b <- cond
            if b then m n' else return n'

    rep p = p `elem` [Rep, RepE, RepNE]

execInstructionBody :: Metadata -> (Bool, Machine ())
execInstructionBody mdat@Metadata{mdInst = i@Inst{..}} = case inOpcode of

    _ | inOpcode `elem` [Ijmp, Icall] -> jump $ do
      case op1 of
        Ptr (Pointer seg (Immediate Bits16 v)) -> do
            when (inOpcode == Icall) $ do
                use cs' >>= push
                use ip' >>= push
            cs .= fromIntegral seg
            ip .= fromIntegral v
        Mem _ -> do
            when (inOpcode == Icall) $ do
                when far $ use cs' >>= push
                use ip' >>= push
            ad <- addr op1
            move ip' $ heap16 ad
            when far $ move cs' $ heap16 $ ad + 2
        _ -> do
            when (inOpcode == Icall) $ do
                use ip' >>= push
            move ip' op1'
      when (inOpcode == Icall) $ do
            x <- use ips
            config . stackTrace %= (x :)

    _ | inOpcode `elem` [Iret, Iretf, Iiretw] -> jump $ do
        when (inOpcode /= Iiretw) $ config . stackTrace %= tail
        when (inOpcode == Iiretw) $ trace_ "iret"
        pop >>= (ip' .=)
        when (inOpcode `elem` [Iretf, Iiretw]) $ pop >>= (cs' .=)
        when (inOpcode == Iiretw) $ pop >>= (flags .=) . (^. ann)
        when (length inOperands == 1) $ op1w >>= (sp %=) . (+) . (^. ann)
--        x <- use ips
{-
      where
        clearStack x [] = []
        clearStack x (a:as)
            | x == a -> as
            | otherwise ->
-}
    Iint  -> jump $ use (byteOperand segmentPrefix op1) >>= interrupt . (^. ann)
    Iinto -> jump $ do
        b <- use overflowF
        when b $ interrupt 4

    Ihlt  -> jump $ throwError Halt

    Ijp   -> jump $ condJump =<< use parityF
    Ijnp  -> jump $ condJump =<< not <$> use parityF
    Ijz   -> jump $ condJump =<< use zeroF
    Ijnz  -> jump $ condJump =<< not <$> use zeroF
    Ijo   -> jump $ condJump =<< use overflowF
    Ijno  -> jump $ condJump =<< not <$> use overflowF
    Ijs   -> jump $ condJump =<< use signF
    Ijns  -> jump $ condJump =<< not <$> use signF

    Ijb   -> jump $ condJump =<< use carryF
    Ijae  -> jump $ condJump =<< not <$> use carryF

    Ijbe  -> jump $ condJump =<< (||) <$> use carryF <*> use zeroF
    Ija   -> jump $ condJump =<< not <$> ((||) <$> use carryF <*> use zeroF)

    Ijl   -> jump $ condJump =<< (/=) <$> use signF <*> use overflowF
    Ijge  -> jump $ condJump =<< (==) <$> use signF <*> use overflowF

    Ijle  -> jump $ condJump =<< (||) <$> ((/=) <$> use signF <*> use overflowF) <*> use zeroF
    Ijg   -> jump $ condJump =<< not <$> ((||) <$> ((/=) <$> use signF <*> use overflowF) <*> use zeroF)

    Ijcxz -> jump $ condJump =<< (== 0) <$> use cx

    Iloop   -> jump $ loop $ return True
    Iloope  -> jump $ loop $ use zeroF
    Iloopnz -> jump $ loop $ not <$> use zeroF

    --------- no jump

    _ -> nojump $ case inOpcode of

        _ | inOpcode `elem` [Iaaa, Iaad, Iaam, Iaas, Idaa, Idas] -> throwError $ Err $ "step: not implemented: " ++ show i
          | length inOperands > 2 -> throwError $ Err "more than 2 operands are not supported"

        Ipusha  -> sequence_ [use r >>= push | r <- [ax',cx',dx',bx',sp',bp',si',di']]
        Ipopa   -> sequence_ [pop >>= (r .=) | r <- [di',si',bp',xx,bx',dx',cx',ax']]
        Ipushfw -> use flags'' >>= push
        Ipopfw  -> pop >>= (flags .=) . (^. ann)

        Iclc  -> carryF     .= False
        Icmc  -> carryF     %= not
        Istc  -> carryF     .= True
        Icld  -> directionF .= False
        Istd  -> directionF .= True
        Icli  -> interruptF .= False
        Isti  -> interruptF .= True

        Inop  -> return ()

        Isahf -> move (flags . low) ah
        Ixlatb -> do
            x <- use al
            move al' $ byteOperand segmentPrefix $ Mem $ Memory undefined (Reg16 RBX) RegNone 0 $ Immediate Bits16 $ fromIntegral x

        Ilea -> op2addr' >>= setOp1 . noAnn
        _ | inOpcode `elem` [Iles, Ilds] -> do
            ad <- addr op2
            move op1' $ heap16 ad
            move (case inOpcode of Iles -> es'; Ilds -> ds') $ heap16 $ ad + 2

        Ipush -> op1w >>= push
        Ipop  -> pop >>= setOp1

        _ -> case sizeByte of
            1 -> withSize byteOperand al' ah' ax'
            2 -> withSize wordOperand ax' dx' dxax

  where
    jump x = (True, x)
    nojump x = (False, x)

    far = " far " `isInfixOf` mdAssembly mdat

    withSize :: forall a . (AsSigned a, Extend a, Extend (Signed a), AsSigned (X2 a), X2 (Signed a) ~ Signed (X2 a))
        => (Maybe Segment -> Operand -> MachinePart (Ann a))
        -> MachinePart (Ann a)
        -> MachinePart (Ann a)
        -> MachinePart (Ann (X2 a))
        -> Machine ()
    withSize tr_ alx ahd axd = case inOpcode of
        Imov  -> move (tr op1) (tr op2)
        Ixchg -> move (uComb (tr op1) (tr op2)) (uComb (tr op2) (tr op1))
        Inot  -> move (tr op1) $ tr op1 . to (annMap "not" complement)

        Isal  -> shiftOp $ \_ x -> (x ^. highBit, x `shiftL` 1)
        Ishl  -> shiftOp $ \_ x -> (x ^. highBit, x `shiftL` 1)
        Ircl  -> shiftOp $ \c x -> (x ^. highBit, x `shiftL` 1 & bit 0 .~ c)
        Irol  -> shiftOp $ \_ x -> (x ^. highBit, x `rotateL` 1)
        Isar  -> shiftOp $ \_ x -> (x ^. bit 0, asSigned x `shiftR` 1 & fromIntegral)
        Ishr  -> shiftOp $ \_ x -> (x ^. bit 0, x `shiftR` 1)
        Ircr  -> shiftOp $ \c x -> (x ^. bit 0, x `shiftR` 1 & highBit .~ c)
        Iror  -> shiftOp $ \_ x -> (x ^. bit 0, x `rotateR` 1)

        Iadd  -> twoOp True  (+)
        Isub  -> twoOp True  (-)
        Icmp  -> twoOp False (-)
        Ixor  -> twoOp True   xor
        Ior   -> twoOp True  (.|.)
        Iand  -> twoOp True  (.&.)
        Itest -> twoOp False (.&.)
        Iadc  -> do
            c <- use carryF
            twoOp True $ \a b -> a + b + fromIntegral (fromEnum c)
        Isbb  -> do
            c <- use carryF
            twoOp True $ \a b -> a - b - fromIntegral (fromEnum c)
        Ineg  -> twoOp_ True (flip (-)) (tr op1) (immLens $ noAnn 0)
        Idec  -> twoOp_ True (+) (tr op1) (immLens $ noAnn (-1))
        Iinc  -> twoOp_ True (+) (tr op1) (immLens $ noAnn 1)

        Idiv  -> divide id id
        Iidiv -> divide asSigned asSigned
        Imul  -> multiply id
        Iimul -> multiply asSigned

        _ | inOpcode `elem` [Icwd, Icbw] -> move axd $ alx . to (annMap "ext" $ fromIntegral . asSigned)
          | inOpcode `elem` [Istosb, Istosw] -> move di'' alx >> adjustIndex di
          | inOpcode `elem` [Ilodsb, Ilodsw] -> move alx si'' >> adjustIndex si
          | inOpcode `elem` [Imovsb, Imovsw] -> move di'' si'' >> adjustIndex si >> adjustIndex di
          | inOpcode `elem` [Iscasb, Iscasw] -> do
            twoOp_ False (-) di'' alx
            adjustIndex di
          | inOpcode `elem` [Icmpsb, Icmpsw] -> do
            twoOp_ False (-) si'' di''
            adjustIndex si
            adjustIndex di

        Iin -> do
            v <- use $ wordOperand segmentPrefix op2
            input (v ^. ann) >>= (tr op1 .=) . fmap fromIntegral
        Iout -> do
            c <- use $ wordOperand segmentPrefix op1
            v <- op2v
            output' (c ^. ann) $ fromIntegral (v ^. ann)

        _ -> error $ "fatal error step: " ++ show i

      where
        si'', di'' :: MachinePart (Ann a)
        si'' = tr $ Mem $ memIndex RSI
        di'' = tr_ (maybe (Just ES) (const $ error "di''") segmentPrefix) $ Mem $ memIndex RDI

        tr :: Operand -> MachinePart (Ann a)
        tr = tr_ segmentPrefix

        divide :: (Integral c, Integral (X2 c)) => (a -> c) -> (X2 a -> X2 c) -> Machine ()
        divide asSigned asSigned' = do
            q <- quotRemSafe <$> (asSigned' <$> use (axd . ann)) <*> (fromIntegral . asSigned . (^. ann) <$> op1v)
            case q of
              Just (d, m) -> do
                alx .= noAnn (fromIntegral d)
                ahd .= noAnn (fromIntegral m)
              Nothing -> throwError Halt

        multiply :: forall c . (Extend c) => (a -> c) -> Machine ()
        multiply asSigned = do
            x <- extend . asSigned <$> use (alx . ann)
            y <- extend . asSigned . (^. ann) <$> op1v
            let r = x * y
                c = r /= extend (fromIntegral r :: c)
            axd .= noAnn (fromIntegral r)
            carryF .= c
            overflowF .= c

        op1v = use $ tr op1
        op2v = use $ tr op2

        shiftOp :: (forall b . (AsSigned b) => Bool -> b -> (Bool, b)) -> Machine ()
        shiftOp op = do
            a <- (^. ann) <$> op1v
            n <- fromIntegral <$> use (byteOperand segmentPrefix op2 . ann)
            case n of
              0 -> do
                return ()
              _ -> do
                c <- use carryF
                let (c'_, r_) = iterate (uncurry op) (c, a) !! (n - 1)
                let (c', r)   = op c'_ r_
                carryF .= c'
                overflowF .= (r ^. highBit /= r_ ^. highBit)
                tr op1 .= noAnn r

        twoOp :: Bool -> (forall b . (Integral b, FiniteBits b) => b -> b -> b) -> Machine ()
        twoOp store op = twoOp_ store op (tr op1) (tr op2)

        twoOp_ :: Bool -> (forall a . (Integral a, FiniteBits a) => a -> a -> a) -> MachinePart (Ann a) -> MachinePart (Ann a) -> Machine ()
        twoOp_ store op op1 op2 = do
            a <- use (op1 . ann)
            b <- use (op2 . ann)
            let r = op a b

            when (inOpcode `notElem` [Idec, Iinc]) $
                carryF .= (fromIntegral r /= op (fromIntegral a :: Int) (fromIntegral b))
            overflowF .= (fromIntegral (asSigned r) /= op (fromIntegral (asSigned a) :: Int) (fromIntegral (asSigned b)))

            zeroF     .= (r == 0)
            signF     .= r ^. highBit
            parityF   .= even (popCount (fromIntegral r :: Word8))
--          adjustF   .= undefined            -- ADC, ADD, CMP, CMPSB, CMPSW, DEC, ...

            when store $ op1 .= noAnn r

    move a b = use b >>= (a .=)

    loop cond = do
        cx %= pred
        condJump =<< (&&) <$> ((/= 0) <$> use cx) <*> cond

    condJump b = when b $ op1w >>= (ip' .=)

    adjustIndex i = do
        d <- use directionF
        i %= if d then (+(-sizeByte)) else (+sizeByte)

    sizeByte :: Word16
    sizeByte = fromIntegral $ sizeByte_ i

    ~(op1: ~(op2:_)) = inOperands

    op1' :: MachinePart (Ann Word16)
    op1' = wordOperand segmentPrefix op1
    setOp1 = (op1' .=)
    op1w = use op1'
    addr op = case op of Mem m -> use $ addressOf segmentPrefix m
    op2addr' = case op2 of Mem m -> use $ addressOf' m

    segmentPrefix :: Maybe Segment
    segmentPrefix = case inPrefixes of
        [Seg s] -> Just s
        [] -> Nothing

input :: Word16 -> Machine (Ann Word16)
input v = do
    case v of
        0x21 -> do
            x <- use intMask
            trace_ $ "get interrupt mask " ++ showHex' 2 x
            return $ "???" @: fromIntegral x
        0x60 -> do
            trace_ "keyboard"
            return $ "???" @: 0
        0x61 -> do
            x <- use $ config . speaker
            trace_ $ "get internal speaker: " ++ showHex' 2 x
            return $ "???" @: fromIntegral x
        0x03da -> do
            r <- getRetrace
            trace_ $ "VGA hardware " ++ showHex' 4 r
            return $ "Vretrace | DD" @: r
        _ -> throwError $ Err $ "input #" ++ showHex' 4 v

output' :: Word16 -> Word16 -> Machine ()
output' v x = do
    case v of
        0x20 -> do
            trace_ $ "int resume " ++ showHex' 2 x  -- ?
            case x of
              0x20 -> setCounter
        0x21 -> do
            trace_ $ "set interrupt mask " ++ showHex' 2 x  -- ?
            intMask .= fromIntegral x
            when (not $ x ^. bit 0) setCounter
        0x40 -> do
            trace_ $ "set timer frequency " ++ showHex' 2 x --show (1193182 / fromIntegral x) ++ " HZ"
        0x41 -> do
            trace_ $ "timer chip 41 " ++ showHex' 2 x  -- ?
        0x43 -> do
            trace_ $ "set timer control " ++ showHex' 2 x
            case x of
                0x36  -> trace_ "set timer frequency lsb+msb, square wave"
                0xb6  -> trace_ "set speaker frequency lsb+msb, square wave"
        0x61 -> do
            config . speaker .= fromIntegral x
            trace_ $ "set internal speaker: " ++ showHex' 2 x
        _ -> throwError $ Err $ "output #" ++ showHex' 4 v ++ " 0x" ++ showHex' 4 x

--------------------------------------------------------

imMax m | IM.null m = 0
        | otherwise = succ . fst . IM.findMax $ m
{-
origInt v = case v of
    0x08 -> return ()
    _ -> throwError $ Err $ "origInt " ++ showHex' 2 v
-}
interrupt_, interrupt, origInterrupt :: Word8 -> Machine ()
interrupt_ n = do
    i <- use interruptF
    if i then interrupt n
         else trace_ $ "interrupt cancelled " ++ showHex' 2 n

origInterrupt = \case

  0x00 -> do
    throwError $ Err $ "int 00"

  0x09 -> do
    throwError $ Err $ "int 09"

  0x16 -> do
    v <- use ah
    case v of
        0x00 -> do
            trace_ "Read (Wait for) Next Keystroke"
            ah' .= "Esc scan code" @: 0x39
            al' .= "Esc ASCII code" @: 0x1b
        0x01 -> do
            trace_ "Query Keyboard Status / Preview Key"
            zeroF .= False  -- no keys in buffer
        v  -> throwError $ Err $ "interrupt #16,#" ++ showHex' 2 v

  0x24 -> do
    throwError $ Err $ "int 24"

  v -> throwError $ Err $ "original interrupt #" ++ showHex' 2 v

interrupt v = case v of

  0x00 -> do
    trace_ "divison by zero interrupt"
    checkInt

  0x08 -> do
    trace_ "timer interrupt again"
    checkInt

  0x09 -> do
    trace_ "keyboard interrupt again"
    checkInt

  0x10 -> do
    trace_ "Video Services"
    v <- use ah
    case v of
        0x00 -> do
            video_mode_number <- use al
            trace_ $ "Set Video Mode #" ++ showHex' 2 video_mode_number
            case video_mode_number of
                0x00 -> do
                    trace_ "text mode"
                0x03 -> do
                    trace_ "mode 3"
                0x13 -> do
                    bx .= 4
                _ -> throwError $ Err $ "#" ++ showHex' 2 video_mode_number
        0x0b -> do
            trace_ "Select Graphics Palette or Text Border Color"

        0x0f -> do
            trace_ "Get Current Video Mode"
            al' .= "text mode" @: 3
            ah' .= "width of screen, in character columns" @: 80
            bh' .= "current active video page (0-based)" @: 0xb8
        0x10 -> do
            trace_ "Set/Get Palette Registers (EGA/VGA)"
            f <- use al
            case f of
              0x12 -> do
                trace_ "set block of DAC color registers"

              v -> throwError $ Err $ "interrupt #10,#10,#" ++ showHex' 2 f

        v  -> throwError $ Err $ "interrupt #10,#" ++ showHex' 2 v

  0x15 -> do
    trace_ "Misc System Services"
    v <- use ah
    case v of
--      0x00 -> do
--        trace_ "Turn on casette driver motor"
      0xc2 -> do
        trace_ "Pointing device BIOS interface"
        w <- use al
        case w of
          0x01 -> do
            trace_ "Reset Pointing device"
            carryF .= False
      v  -> throwError $ Err $ "interrupt #15,#" ++ showHex' 2 v

  0x16 -> do
   trace_ "Keyboard Services"
   checkInt

  0x20 -> do
    trace_ "halt"
    throwError Halt

  0x21 -> do
    trace_ "DOS rutine"
    v <- use ah
    case v of
        0x00 -> do
            trace_ "Program terminate"
            throwError Halt

        0x1a -> do
            trace_ "Set Disk Transfer Address (DTA)"
            addr <- use $ addressOf Nothing $ memIndex RDX
            dta .= addr

        0x25 -> do
            v <- fromIntegral <$> use al     -- interrupt vector number
            trace_ $ "Set Interrupt Vector " ++ showHex' 2 v
            use dx' >>= (heap16 (4*v) .=)     -- DS:DX = pointer to interrupt handler
            use ds' >>= (heap16 (4*v + 2) .=)

        0x30 -> do
            trace_ "Get DOS version"
            al' .= "major version number" @: 0x04      --  (2-5)
            ah' .= "minor version number" @: 0x00      --  (in hundredths decimal)
            bh' .= "MS-DOS" @: 0xff
            do              -- 24 bit OEM serial number
                bl' .= "OEM serial number (high bits)" @: 0
                cx' .= "OEM serial number (low bits)" @: 0

        0x35 -> do
            v <- fromIntegral <$> use al     -- interrupt vector number
            trace_ $ "Get Interrupt Vector " ++ showHex' 2 v
            use (heap16 (4*v)) >>= (bx' .=)
            use (heap16 (4*v+2)) >>= (es' .=)   -- ES:BX = pointer to interrupt handler

        0x3d -> do
            trace_ "Open File Using Handle"
            open_access_mode <- use al
--            v <- use dx
            case open_access_mode of
              0 -> do   -- read mode
                addr <- use $ addressOf Nothing $ memIndex RDX
                fname <- use $ heap . bytesAt addr 20
                let f = map (toUpper . chr . fromIntegral) $ takeWhile (/=0) fname
                trace_ $ "File: " ++ show f
                let fn = "../original/" ++ f
                let s = unsafePerformIO $ do
                        b <- doesFileExist fn
                        if b then Just <$> BS.readFile fn else return Nothing
                case s of
                  Nothing -> do
                    trace_ $ "not found"
                    ax' .= "File not found" @: 0x02
                    carryF .= True
                  Just s -> do
        --            ax .= 02  -- File not found
                    handle <- max 3 . imMax <$> use files
                    trace_ $ "handle " ++ showHex' 4 handle
                    files %= IM.insert handle (fn, s, 0)
                    ax' .= "file handle" @: fromIntegral handle
                    carryF .= False

        0x3e -> do
            trace_ "Close file"
            handle <- fromIntegral <$> use bx
            trace_ $ "handle " ++ showHex' 4 handle
            x <- IM.lookup handle <$> use files
            case x of
              Just (fn, _, _) -> do
                trace_ $ "file: " ++ fn
                files %= IM.delete handle
                carryF .= False

        0x3f -> do
            handle <- fromIntegral <$> use bx
            fn <- (^. _1) . (IM.! handle) <$> use files
            num <- fromIntegral <$> use cx
            trace_ $ "Read " ++ showHex' 4 handle ++ ":" ++ fn ++ " " ++ showHex' 4 num
            loc <- use $ addressOf Nothing $ memIndex RDX
            s <- BS.take num . (\(fn, s, p) -> BS.drop p s) . (IM.! handle) <$> use files
            let len = BS.length s
            files %= flip IM.adjust handle (\(fn, s, p) -> (fn, s, p+len))
            heap . bytesAt loc len .= BS.unpack s
            ax' .= "length" @: fromIntegral len
            carryF .= False

        0x40 -> do
            handle <- fromIntegral <$> use bx
            trace_ $ "Write to " ++ showHex' 4 handle
            num <- fromIntegral <$> use cx
            loc <- use $ addressOf Nothing $ memIndex RDX
            case handle of
              1 -> trace_ . ("STDOUT: " ++) . map (chr . fromIntegral) =<< use (heap . bytesAt loc num)
              2 -> trace_ . ("STDERR: " ++) . map (chr . fromIntegral) =<< use (heap . bytesAt loc num)
              _ -> return ()
            carryF .= False

        0x42 -> do
            handle <- fromIntegral <$> use bx
            fn <- (^. _1) . (IM.! handle) <$> use files
            mode <- use al
            pos <- fromIntegral . asSigned <$> use (uComb cx dx . combine)
            trace_ $ "Seek " ++ showHex' 4 handle ++ ":" ++ fn ++ " to " ++ show mode ++ ":" ++ showHex' 8 pos
            files %= (flip IM.adjust handle $ \(fn, s, p) -> case mode of
                0 -> (fn, s, pos)
                1 -> (fn, s, p + pos)
                2 -> (fn, s, BS.length s + pos)
                )
            pos' <- (^. _3) . (IM.! handle) <$> use files
            (uComb dx ax . combine) .= fromIntegral pos'
            carryF .= False

        0x44 -> do
            trace_ "I/O Control for Devices (IOCTL)"
            0x44 <- use ah
            function_value <- use al
{-
            file_handle <- use bx
            logical_device_number <- use bl -- 0=default, 1=A:, 2=B:, 3=C:, ...
            number_of_bytes_to_read_or_write <- use cx
            data_or_buffer <- use dx
-}
            case function_value of
              0x00 -> do
                handle <- use bx
                trace_ $ "Get Device Information of " ++ show handle 
                case handle of
                  4 -> dx .= 0x2804        --  0010 1000 00 000100   no D: drive
                  3 -> dx .= 0x2883        --  0010 1000 00 000011   no C: drive
                  2 -> dx .= 0x2802        --  0010 1000 00 000011    B: drive
                  1 -> dx .= 0x2801        --  0010 1000 00 000011    A: drive
                  0 -> dx .= 0x2800        --  0010 1000 00 000011    default drive
            carryF .= False

        0x48 -> do
            memory_paragraphs_requested <- use bx
            trace_ $ "Allocate Memory " ++ showHex' 5 (memory_paragraphs_requested ^. paragraph)
            x <- zoom heap $ extendMem $ memory_paragraphs_requested + 1
            ax' .= "segment address of allocated memory block" @: x + 1 -- (MCB + 1para)
            use heap >>= trace_ . showMem
            carryF .= False

        0x4a -> do
            trace_ "Modify allocated memory blocks"
            new_requested_block_size_in_paragraphs <- use bx
            segment_of_the_block <- use es      -- (MCB + 1para)
            Overlay ram rom <- use heap
--            throwError $ Err $ showRom rom
            heap .= Overlay ram (extendRom' (segment_of_the_block ^. paragraph) (new_requested_block_size_in_paragraphs ^. paragraph) rom)
            use heap >>= trace_ . showMem
            carryF .= False      -- unlimited memory available

        0x4c -> do
            code <- use al
            trace_ $ "Terminate Process With Return Code #" ++ showHex' 2 code
            throwError Halt

        0x4e -> do
            attribute_used_during_search <- use cx
            addr <- use $ addressOf Nothing $ memIndex RDX
            fname <- use $ heap . bytesAt addr 20
            let f_ = map (chr . fromIntegral) $ takeWhile (/=0) fname
            trace_ $ "Find file " ++ show f_
            ad <- use dta
--            throwError Halt

            let s = unsafePerformIO $ do
                    b <- globDir1 (compile $ map toUpper f_) "../original"
                    case b of
                        (f:_) -> Just . (,) f <$> BS.readFile f
                        _ -> return Nothing
            case s of
              Just (f, s) -> do
                trace_ $ "found: " ++ show f
                heap . bytesAt 0 0x1a .= map (error . ("undefined byte " ++) . showHex' 2) [0..]
                heap8 0x00 .= "attribute of serach" @: fromIntegral attribute_used_during_search
                heap8 0x01 .= "disk used during search" @: 2  -- C:
                heap . bytesAt 0x02 11 .= fname
                heap . dwordAt (ad + 0x1a) .= fromIntegral (BS.length s)
                heap . bytesAt (ad + 0x1e) 13 .= map (fromIntegral . ord) (takeFileName f) ++ [0]
                carryF .= False
              Nothing -> do
                trace_ $ "not found"
                ax .= 02  -- File not found
                carryF .= True

        0x62 -> do
            trace_ "Get PSP address (DOS 3.x)"
            bx' .= "segment address of current process" @: 0x100 - 0x10  -- hack!!!
            carryF .= False

        _    -> throwError $ Err $ "dos function #" ++ showHex' 2 v

  0x24 -> do
    trace_ "critical error handler interrupt"
    checkInt

  0x33 -> do
    trace_ "Mouse Services"
    v <- use ax
    case v of
        0x00 -> do
            trace_ "Mouse Reset/Get Mouse Installed Flag"
            ax' .= "mouse driver not installed" @: 0x0000
            bx' .= "number of buttons" @: 0
        0x03 -> do
            trace_ "Get Mouse position and button status"
            cx' .= "mouse X" @: 0
            dx' .= "mouse Y" @: 0
            bx' .= "button status" @: 0
        _    -> throwError $ Err $ "#" ++ showHex' 2 v

  v -> throwError $ Err $ "interrupt #" ++ showHex' 2 v
 where
    checkInt :: Machine ()
    checkInt = do

        lo <- use $ heap16 (4*fromIntegral v)
        hi <- use $ heap16 (4*fromIntegral v + 2)
        case (hi ^. ann) of
          0xffff -> origInterrupt $ fromIntegral (lo ^. ann)
          _ -> do
            trace_ $ "user interrupt " ++ showHex' 2 v
            use flags >>= push . noAnn
            use cs' >>= push
            use ip' >>= push
            interruptF .= False
            cs' .= hi
            ip' .= lo



timerInt = do
    trace_ "timer"
    mask <- use intMask
    when (not (mask ^. bit 0)) $ interrupt_ 0x08
        

----------------------------------------------

infixl 9 @.
m @. i = push_ i >> m

class PushVal a where
    push_ :: a -> Machine ()
instance PushVal Word16 where
    push_ = push . noAnn

----------------------------------------------

prelude1, prelude :: [Word8]
prelude1
     = [error $ "interruptTable " ++ showHex' 2 (i `div` 4) | i <- [0..1023]]
    ++ replicate 172 (error "BIOS communication area")
    ++ replicate 68 (error "reserved by IBM")
    ++ replicate 16 (error "user communication area")
    ++ replicate 256 (error "DOS communication area")
    ++ [error $ "dos area " ++ showHex' 2 i | i <- [0x600 ..0x700-1]]
prelude
     = prelude1
    ++ origTimer
    ++ [error $ "dos area " ++ showHex' 2 i | i <- [0x700 + length origTimer..0x800-1]]

origTimer =
    [0x50, 0xb0, 0x20, 0xe6, 0x20, 0x58, 0xcf]       -- push ax; mov al, 20h; out 20h, al; pop ax; iret
    ++ replicate (maxInstLength - 1) 0  -- hack for fetchinstruction

loadTest :: BS.ByteString -> MachineState
loadTest com = flip execState emptyState $ do
    heap .= toRom (concat
        [ replicate 0xf0000 0
        , BS.unpack com
        , replicate 64 0 -- extra bytes for decoding
        ])
    cs .= loadSegment
    ds .= loadSegment
    es .= loadSegment
    ip .= 0xfff0
    ax .= 0
    bx .= 0
    cx .= 1
    di .= 0
    si .= 0

    ds .= 0
    ss .= stackSegment
    sp .= 0

    flags .= 0x0057

    clearHist
  where
    loadSegment = 0xf000
    stackSegment = 0 --0xe000

loadCom :: BS.ByteString -> MachineState
loadCom com = flip execState emptyState $ do
    heap .= toRom (concat
        [ prelude
        , replicate (loadSegment ^. paragraph - length prelude) 0
        , BS.unpack com
        , replicate (l' - l + stacksize + 2^16) 0
        ])

    cs .= loadSegment
    ds .= loadSegment
    es .= loadSegment
    ip .= 0x0100
    ax .= 0
    bx .= 0
    cx .= 1
    di .= 0xfffe
    si .= 0x0100

    forM_ [0xfff4..0xffff] $ \i -> do
        heap8 (segAddr gs i) .= "junk" @: 1
    heap8 (segAddr gs 0x20cd) .= "junk" @: 1

    ss .= l' ^. from paragraph
    sp .= fromIntegral stacksize
    heap16 4 .= "???" @: 0
    heap16 6 .= "segment" @: gs

    clearHist
  where
    l = BS.length com + loadSegment ^. paragraph
    l' = align 16 l
    gs = (l' + stacksize) ^. from paragraph

    stacksize = 2^8 :: Int

    loadSegment = 0x100

programSegmentPrefix :: Word16 -> Word16 -> BS.ByteString -> IM.IntMap (Ann Word8)
programSegmentPrefix envseg endseg args = flip execState (toRom $ map (error . ("psp uninitialized byte: " ++) . showHex' 2) [0..0xff]) $ do

    wordAt' 0x00 .= "CP/M exit, always contain code 'int 20h'" @: 0x20CD
    wordAt' 0x02 .= "Segment of the first byte beyond the memory allocated to the program" @: endseg
--    bytesAt 0x05 5 .= [0xea, 0xff, 0xff, 0xad, 0xde]   -- FAR call to MSDOS function dispatcher (int 21h)?
--    dwordAt 0x0a .= 0xf00020c8    -- Terminate address of previous program (old INT 22h)
--    dwordAt 0x0e .= 0x01180000    -- Break address of previous program (old INT 23h)
--    dwordAt 0x12 .= 0x01180110    -- Critical error address of previous program (old INT 24h)
--    wordAt 0x16 .= 0x0118    -- Caller's PSP segment (usually COMMAND.COM - internal)

    -- Job File Table (JFT) (internal)
--    bytesAt 0x18 20 .= [0x01, 0x01, 0x01, 0x00, 0x02, 0x03] ++ repeat 0xff

    wordAt' 0x2c .= "Environment segment" @: envseg
--    dwordAt 0x2e .= 0x0192ffe6 -- SS:SP on entry to last INT 21h call (internal)

--    wordAt 0x32 .= 0x0014 -- JFT size (internal)
--    dwordAt 0x34 .= 0x01920018-- Pointer to JFT (internal)
--    dwordAt 0x38 .= 0xffffffff -- Pointer to previous PSP (only used by SHARE in DOS 3.3 and later)
    -- 3Ch-3Fh     4 bytes     Reserved
--    wordAt 0x40 .= 0x0005 -- DOS version to return (DOS 4 and later, alterable via SETVER in DOS 5 and later)
    -- 42h-4Fh     14 bytes     Reserved
    bytesAt 0x50 3 .= [0xcd, 0x21, 0xcb] -- (code) Far call to DOS (always contain INT 21h + RETF)
    -- 53h-54h     2 bytes     Reserved
    -- 55h-5Bh     7 bytes     Reserved (can be used to make first FCB into an extended FCB)

    -- 5Ch-6Bh     16 bytes     Unopened Standard FCB 1
    -- 6Ch-7Fh     20 bytes     Unopened Standard FCB 2 (overwritten if FCB 1 is opened)
--    bytesAt 0x5c (16 + 20) .= repeat 0

    byteAt' 0x80 .= "args length" @: fromIntegral (min maxlength $ BS.length args)
    bytesAt 0x81 (maxlength + 1) .= take maxlength (BS.unpack args) ++ [0x0D]  -- Command line string
--    byteAt 0xff .= 0x36   -- dosbox specific?
  where
    maxlength = 125

pspSize = 256 :: Int

--
envvars :: [Word8]
envvars = map (fromIntegral . ord) "COMSPEC=C:\\cmd.exe\NULPATH=C:\NULPROMPT=$P\NUL\NUL\NULC:\\game.exe\NUL" ++ replicate 20 0

--envvars = [0,0,0,0,0] --"\NUL\NUL\NUL\NUL\NUL\NUL" -- BS.concat (map (`BS.append` "\NUL") ["PATH="]) `BS.append` "\NUL"

loadExe :: IM.IntMap BS.ByteString -> Word16 -> BS.ByteString -> MachineState
loadExe labs loadSegment gameExe = flip execState emptyState $ do
    heap .= Overlay (toRom [])
        (Split 0xa0000
            (toRom $ concat
                [ prelude
                , envvars
                , replicate (loadSegment ^. paragraph - length prelude - length envvars - pspSize) $ error "dos internals 2"
                , fromRom 0 256 $ programSegmentPrefix (length prelude ^. from paragraph) endseg ""
                , BS.unpack $ relocate relocationTable loadSegment $ BS.drop headerSize gameExe
                , replicate (additionalMemoryAllocated ^. paragraph) (fromIntegral $ ord '?')
                ])
            (Zeros True 0xa0000 0x10000)
        )
    ss .= ssInit + loadSegment
    sp .= spInit
    cs .= csInit + loadSegment
    ip .= ipInit
    ds .= pspSegment
    es .= pspSegment
    labels .= IM.fromDistinctAscList (map ((+ reladd) *** id) $ IM.toList labs)

    mapM_ inter [0x00, 0x09, 0x16, 0x24]
    inter' 0x08

    heap16 0x410 .= "equipment word" @: 0x4463
    heap8 0x417 .= "keyboard shift flag 1" @: 0x20

    clearHist
  where
    inter' i = do
        heap16 (4*i) .= "interrupt lo" @: 0
        heap16 (4*i+2) .= "interrupt hi" @: (length prelude1 ^. from paragraph)

    inter i = do
        heap16 (4*i) .= "interrupt lo" @: fromIntegral i
        heap16 (4*i+2) .= "interrupt hi" @: 0xffff

    reladd = loadSegment ^. paragraph

    pspSegment = loadSegment - (pspSize ^. from paragraph)
    endseg = loadSegment + executableSize ^. from paragraph + additionalMemoryAllocated

    additionalMemoryAllocated = maxAdditionalMemoryNeeded
        -- could be anything between additionalMemoryNeeded and maxAdditionalMemoryNeeded

    (0x5a4d: bytesInLastPage: pagesInExecutable: relocationEntries:
     paragraphsInHeader: additionalMemoryNeeded: maxAdditionalMemoryNeeded: ssInit:
     spInit: checksum: ipInit: csInit:
     firstRelocationItemOffset: overlayNumber: headerLeft)
        = map (\[low, high] -> (high, low) ^. combine) $ everyNth 2 $ BS.unpack $ gameExe

    headerSize = paragraphsInHeader ^. paragraph
    executableSize = (fromIntegral pagesInExecutable `shiftL` 9)
            + if (bytesInLastPage > 0) then fromIntegral bytesInLastPage - 0x200 else 0 :: Int

    relocationTable = sort $ take (fromIntegral relocationEntries)
        $ map (\[a,b]-> segAddr b a) $ everyNth 2 $ drop (fromIntegral firstRelocationItemOffset `div` 2 - 14) headerLeft

relocate :: [Int] -> Word16 -> BS.ByteString -> BS.ByteString
relocate table loc exe = BS.concat $ fst: map add (bss ++ [last])
  where
    (last, fst: bss) = mapAccumL (flip go) exe $ zipWith (-) table $ 0: table

    go r (BS.splitAt r -> (xs, ys)) = (ys, xs)

    add (BS.uncons -> Just (x, BS.uncons -> Just (y, xs))) = BS.cons x' $ BS.cons y' xs
        where (y',x') = combine %~ (+ loc) $ (y,x)


----------------------------------- dead code

{-
instance Rom (S.Seq Word8) where
    toRom = S.fromList
    fromRom i j = take j . seqToList . S.drop i
    readByte_ s i | 0 <= i && i < S.length s = Just $ S.index s i
                  | otherwise = Nothing
instance Ram (S.Seq Word8) where
    writeByte = S.update

instance Rom BS.ByteString where
    toRom = BS.pack
    fromRom i j = take j . BS.unpack . BS.drop i
    readByte_ s i | 0 <= i && i < BS.length s = Just $ BS.index s i
                  | otherwise = Nothing

instance ExtRom BS.ByteString where
    extendRom x v = 
        (len' ^. from paragraph, v `BS.append` BS.replicate ((len' - len) + (x + 1) ^. paragraph) 0)
      where
        len = BS.length v
        len' = align 16 len

instance Ram (V.Vector Word8) where
    writeByte i b x = x V.// [(i,b)]
-}
{-
instance PushVal (V.Vector Word16) where
    push_ v = do
        x_ <- sLength <$> use heap
        s <- use ds
        push $ fromIntegral $ x_ - s ^. paragraph
        heap %= (`sJoin` h)
      where
        h = toRom $ concatMap (\w -> [w ^. low, w ^. high]) $ V.toList v
-}
{-
findRet n = do
    op <- step_
    case inOpcode $ mdInst op of
        Iretf | n == 0 -> return ()
        Iretf -> findRet (n-1)
        Icall -> findRet (n+1)
        _ -> findRet n
-}

