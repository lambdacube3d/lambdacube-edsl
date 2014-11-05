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
    | Low Annot
    | High Annot
    | CombAnn Annot Annot
    | Annot BS.ByteString
    deriving (Eq, Ord)

data Ann a = Ann
    { _annot :: Annot
    , _ann :: a
    }

$(makeLenses ''Ann)

showAnn NoAnnot = ""
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
    low (CombAnn a b) = a
    low a = Low a
    high NoAnnot = NoAnnot
    high (CombAnn a b) = b
    high a = High a
    combAnn NoAnnot NoAnnot = NoAnnot
    combAnn (Low a) (High a') | a == a' = a
    combAnn a b = CombAnn a b

clearAnn = iso (^. ann) $ Ann NoAnnot

noAnn = Ann NoAnnot
b @: x = Ann (Annot b) x
infix 5 @:

instance (Eq a) => Eq (Ann a) where
    a == b = a ^. ann == b ^. ann

instance Show a => Show (Ann a) where
    show (Ann _ a) = show a

instance Functor Ann where
    fmap f (Ann x y) = Ann x (f y)

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
    | Zeros Int Int

showRom :: ERom -> String
showRom (Rom v) = "Rom " ++ showHex' 5 (V.length v)
showRom (Split i a b) = "Split " ++ showHex' 5 i ++ " (" ++ showRom a ++ ") (" ++ showRom b ++ ")"
showRom (Zeros i j) = "Zeros " ++ showHex' 5 i ++ " " ++ showHex' 5 j

extendRom' :: Int -> Int -> ERom -> ERom
extendRom' beg size r@(Rom v)
    | beg + size <= V.length v = r
    | beg <= V.length v = Split (V.length v) r $ Zeros (V.length v) $ beg + size - V.length v
extendRom' beg size (Split i a b)
    | beg + size <= i = Split i (extendRom' beg size a) b
    | beg >= i = Split i a (extendRom' beg size b)
--    | beg <= V.length v = Split (V.length v) r $ Zeros (V.length v) $ beg + size - 1
--extendRom' beg size (Zeros a b)
extendRom' beg size (Zeros a b)
    | a <= beg && beg <= a + b = Zeros a $ max b $ beg - a + size
extendRom' a b r = error $ "extendRom' " ++ showHex' 5 a ++ " " ++ showHex' 5 b ++ " (" ++ showRom r ++ ")"


instance Rom ERom where
    readByte_ (Rom a) i = readByte_ a i
    readByte_ (Zeros a b) i
        | a <= i && i < a + b = Just (noAnn 0) -- $ error "mem uninitialized byte") --noAnn 0)
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
        f u w@(Zeros a b) = [add x w | let x = align' (a + b), x + needed <= u ]
        f u w@(Rom v) = [add x w | let x = align' $ V.length v, x + needed <= u]
        align' = align 16

        add len v = (len, Split len v (Zeros len $ x ^. paragraph))

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
    , _verboseLevel :: Int
    }

$(makeLenses ''Config_)

defConfig = Config_
    { _numOfDisasmLines = 30
    , _verboseLevel = 2
    }

data MachineState = MachineState
    { _flags'   :: Word16
    , _regs     :: IM.IntMap (Ann Word8)
    , _heap     :: Overlay (IM.IntMap (Ann Word8)) ERom

    , _hist     :: Set.Set Key
    , _traceQ   :: [String]
    , _config   :: Config_
    , _cache    :: IM.IntMap (Machine ())
    , _labels   :: IM.IntMap BS.ByteString
    , _files    :: IM.IntMap (FilePath, BS.ByteString, Int)  -- filename, file, position
    , _dta      :: Int
    }

emptyState = MachineState
    { _flags'   = 0x7202
    , _regs     = toRom (replicate 26 0)
    , _heap     = toRom []

    , _hist     = Set.empty
    , _traceQ   = []
    , _config   = defConfig
    , _cache    = IM.empty
    , _labels   = IM.empty
    , _files    = IM.empty
    , _dta      = 0
    }

data Halt
    = Halt
    | Err String
  deriving Show

instance Error Halt where

type Machine = ErrorT Halt (State MachineState)

$(makeLenses ''MachineState)

trace_ :: String -> Machine ()
trace_ s = traceQ %= (s:)

steps = config . numOfDisasmLines

clearHist = do
    hist .= Set.empty
    traceQ .= []

type MachinePart a = Lens' MachineState a

addHist :: Key -> MachinePart a -> MachinePart a
addHist sh k = lens (^. k) $ \s a -> hist %~ Set.insert sh $ set k a s

flagLenses@[_,_,_,_,overflowF,directionF,interruptF,trapF,signF,zeroF,_,adjustF,_,parityF,_,carryF]
    = [ addHist (Flag i) $ flags' . bit i | i <- [15,14..0] ]
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

flags = addHist Flags flags'

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
        ] ++ map (take 149) (take' (s ^. config . numOfDisasmLines) $ showCode s)
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

showCode :: MachineState -> [String]
showCode s = case x of
    Left e -> ("  " ++ show e): []
    Right x -> case s ^. config . verboseLevel of 
      1 -> 
        (  ifff (intercalate "; " (reverse (s' ^. traceQ)))
        ++ case y of
        Left e -> show e: []
        Right () -> showCode s'
        )
      2 -> 
        maybeToList (BSC.unpack <$> IM.lookup (s ^. ips) (s ^. labels))
         ++
        (("  " ++ pad 14 (map toUpper $ mdHex x) ++ " "
        ++ pad 27 (showInst s x) ++ "" ++ intercalate "; " (reverse (s' ^. traceQ))
        ++ "  " ++ unwords (map shKey $ combineKey $ Set.toList $ s' ^. hist)): case y of
        Left e -> show e: []
        Right () -> showCode s'
        )
 where
    (x, s_) = flip runState s $ runErrorT $ fetchInstr

    (y, s') = flip runState s_ $ runErrorT $ do
        clearHist
        snd $ execInstruction $ either (error . show) mdInst $ x

    shKey k = case k of
        Heap 1 i  -> diff (sh' 1) $ heap . byteAt i
        Heap 2 i  -> diff (sh' 2) $ heap . wordAt i
        Heap n i  -> "[" ++ showHex' 5 i ++ "-" ++ showHex' 5 (i+n-1) ++ "]"
        Flag i    -> diff sf $ flags . bit i
        Flags     -> diff showFlags flags
        KReg 1 i  -> diff (sh'' 1) $ regs . byteAt' i
        KReg 2 i  -> diff (sh 2) $ regs . wordAt i
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

        sh' :: (Show a, Integral a) => Int -> a -> String
        sh' i v = lok i (show k) ++ ":" ++ showHex' (2*i) v

        sh'' :: (Show a, Integral a) => Int -> Ann a -> String
        sh'' i v = lok i (show k) ++ ":" ++ showA (showHex' (2*i) <$> v)

        sf True = map toUpper $ show k
        sf False = show k

        par True a = "(" ++ a ++ ")"
        par False a = a

sizeByte_ i@Inst{..}
    | inOpcode `elem` [Icmpsb, Icbw, Imovsb, Istosb, Ilodsb, Iscasb] = 1
    | inOpcode `elem` [Icmpsw, Icwd, Imovsw, Istosw, Ilodsw, Iscasw] = 2
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
    Reg16 RDI -> es
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
    ips <- use ips
    Just (md, _) <- disassembleOne disasmConfig . BS.pack . fromRom ips 20 <$> use heap
    ip %= (+ fromIntegral (mdLength md))
    return md

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
                let (jump, m_) = execInstruction $ mdInst md
                    m = ip .= ip' >> m_
                m_
                (m >>) <$> if jump
                  then return (return ())
                  else collect

        m <- collect
        cache %= IM.insert ips m

execInstruction :: Instruction -> (Bool, Machine ())
execInstruction i@Inst{..} = case filter nonSeg inPrefixes of
    [Rep, RepE]
        | inOpcode `elem` [Icmpsb, Icmpsw, Iscasb, Iscasw] -> (,) jump $ cycle $ use zeroF      -- repe
        | otherwise -> (,) jump cycle'      -- rep
    [RepNE] -> (,) jump $ cycle $ not <$> use zeroF
    [] -> (,) jump body
    x -> error $ "execInstruction: " ++ show x
  where
    (jump, body) = execInstructionBody $ i { inPrefixes = filter (not . rep) inPrefixes }

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

execInstructionBody :: Instruction -> (Bool, Machine ())
execInstructionBody i@Inst{..} = case inOpcode of

    _ | inOpcode `elem` [Ijmp, Icall] -> jump $ case op1 of
        Ptr (Pointer seg (Immediate Bits16 v)) -> do
            when (inOpcode == Icall) $ do
                use cs' >>= push
                use ip' >>= push
            cs .= fromIntegral seg
            ip .= fromIntegral v
        Mem _ -> do
            when (inOpcode == Icall) $ do
                use cs' >>= push
                use ip' >>= push
            ad <- addr op1
            move ip' $ heap16 ad
            move cs' $ heap16 $ ad + 2
        _ -> do
            when (inOpcode == Icall) $ do
                use ip' >>= push
            move ip' op1'

    _ | inOpcode `elem` [Iret, Iretf, Iiretw] -> jump $ do
        pop >>= (ip' .=)
        when (inOpcode `elem` [Iretf, Iiretw]) $ pop >>= (cs' .=)
        when (inOpcode == Iiretw) $ pop >>= (flags .=) . (^. ann)
        when (length inOperands == 1) $ op1w >>= (sp %=) . (+) . (^. ann)

    Iint  -> jump $ use (byteOperand segmentPrefix op1) >>= interrupt . (^. ann)
    Iinto -> jump $ do
        b <- use overflowF
        when b $ interrupt 4

    Ihlt  -> jump $ throwError Halt

    Ija   -> jump $ condJump =<< (&&) <$> (not <$> use carryF) <*> (not <$> use zeroF)
    Ijae  -> jump $ condJump =<< not <$> use carryF
    Ijb   -> jump $ condJump =<< use carryF
    Ijbe  -> jump $ condJump =<< (||) <$> use carryF <*> use zeroF
    Ijcxz -> jump $ condJump =<< (== 0) <$> use cx
    Ijg   -> jump $ condJump =<< (&&) <$> ((==) <$> use signF <*> use overflowF) <*> (not <$> use zeroF)
    Ijge  -> jump $ condJump =<< (==) <$> use signF <*> use overflowF
    Ijl   -> jump $ condJump =<< (/=) <$> use signF <*> use overflowF
    Ijle  -> jump $ condJump =<< (||) <$> ((/=) <$> use signF <*> use overflowF) <*> use zeroF
    Ijno  -> jump $ condJump =<< not <$> use overflowF
    Ijnp  -> jump $ condJump =<< not <$> use parityF
    Ijns  -> jump $ condJump =<< not <$> use signF
    Ijnz  -> jump $ condJump =<< not <$> use zeroF
    Ijo   -> jump $ condJump =<< use overflowF
    Ijp   -> jump $ condJump =<< use parityF
    Ijs   -> jump $ condJump =<< use signF
    Ijz   -> jump $ condJump =<< use zeroF
    Iloop   -> jump $ loop $ return True
    Iloope  -> jump $ loop $ use zeroF
    Iloopnz -> jump $ loop $ not <$> use zeroF

    --------- no jump

    _ -> nojump $ case inOpcode of

        _ | inOpcode `elem` [Iaaa, Iaad, Iaam, Iaas, Idaa, Idas] -> throwError $ Err $ "step: not implemented: " ++ show i
          | length inOperands > 2 -> throwError $ Err "more than 2 operands are not supported"

        Ipusha  -> sequence_ [use r >>= push | r <- [ax',cx',dx',bx',sp',bp',si',di']]
        Ipopa   -> sequence_ [pop >>= (r .=) | r <- [di',si',bp',xx,bx',dx',cx',ax']]
        Ipushfw -> use flags >>= push . noAnn
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
            1 -> withSize (byteOperand segmentPrefix) al' ah' ax'
            2 -> withSize (wordOperand segmentPrefix) ax' dx' dxax

  where
    jump x = (True, x)
    nojump x = (False, x)

    withSize :: forall a . (AsSigned a, AsSigned (X2 a), X2 (Signed a) ~ Signed (X2 a))
        => (Operand -> MachinePart (Ann a))
        -> MachinePart (Ann a)
        -> MachinePart (Ann a)
        -> MachinePart (Ann (X2 a))
        -> Machine ()
    withSize tr alx ahd axd = case inOpcode of
        Imov  -> move (tr op1) (tr op2)
        Ixchg -> move (uComb (tr op1) (tr op2)) (uComb (tr op2) (tr op1))
        Inot  -> move (tr op1) $ tr op1 . to (annMap "not" complement)      -- ann compl!

        Isal  -> shiftOp $ \_ x -> (x ^. highBit, x `shiftL` 1)
        Ishl  -> shiftOp $ \_ x -> (x ^. highBit, x `shiftL` 1)
        Ircl  -> shiftOp $ \c x -> (x ^. highBit, bit 0 .~ c $ x `shiftL` 1)
        Irol  -> shiftOp $ \_ x -> (x ^. highBit, x `rotateL` 1)
        Isar  -> shiftOp $ \_ x -> (x ^. bit 0, fromIntegral $ asSigned x `shiftR` 1)
        Ishr  -> shiftOp $ \_ x -> (x ^. bit 0, x `shiftR` 1)
        Ircr  -> shiftOp $ \c x -> (x ^. bit 0, highBit .~ c $ x `shiftR` 1)
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

        _ | inOpcode `elem` [Icwd, Icbw] -> move axd $ alx . to (annMap "ext" $ fromIntegral . asSigned)        -- ann ext!
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
        di'' = tr $ Mem $ memIndex RDI

        divide :: (Integral c, Integral (X2 c)) => (a -> c) -> (X2 a -> X2 c) -> Machine ()
        divide asSigned asSigned' = do
            q <- quotRemSafe <$> (asSigned' <$> use (axd . ann)) <*> (fromIntegral . asSigned . (^. ann) <$> op1v)
            case q of
              Just (d, m) -> do
                alx .= noAnn (fromIntegral d)
                ahd .= noAnn (fromIntegral m)
              Nothing -> throwError Halt

        multiply :: (Integral c) => (a -> c) -> Machine ()
        multiply asSigned = do
            x <- asSigned <$> use (alx . ann)
            y <- asSigned . (^. ann) <$> op1v
            let r = fromIntegral x * fromIntegral y :: Int
                c = r == fromIntegral (x * y)
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
            parityF   .= even (popCount r)
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
            trace_ "interrupt control port" -- ?
            return $ "???" @: 0xff  -- 
        0x60 -> do
            trace_ "keyboard"
            return $ "???" @: 0
        0x61 -> do
            trace_ "internal speaker"
            return $ "???" @: (-1)
        _ -> throwError $ Err $ "input #" ++ showHex' 4 v

output' :: Word16 -> Word16 -> Machine ()
output' v x = do
    case v of
        0x21 -> do
            trace_ "interrupt control port"  -- ?
            return ()
        0x40 -> do
            trace_ "timer chip"  -- ?
            return ()
        0x41 -> do
            trace_ "timer chip"  -- ?
            return ()
        0x43 -> do
            trace_ "timer chip"  -- ?
            return ()
        0x61 -> do
            trace_ "internal speaker"
            return ()
        _ -> throwError $ Err $ "output #" ++ showHex' 4 v ++ " 0x" ++ showHex' 4 x

--------------------------------------------------------

imMax m | IM.null m = 0
        | otherwise = succ . fst . IM.findMax $ m

interrupt 0x10 = do
    trace_ "Video Services"
    v <- use ah
    case v of
        0x00 -> do
            trace_ "Set Video Mode"
            video_mode_number <- use al
            case video_mode_number of
                0x00 -> do
                    trace_ "text mode"
                0x13 -> do
                    bx .= 4
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

interrupt 0x15 = do
    trace_ "Misc System Services"
    v <- use ah
    case v of
        0x00 -> do
            trace_ "Turn on casette driver motor"
        v  -> throwError $ Err $ "interrupt #15,#" ++ showHex' 2 v

interrupt 0x16 = do
    trace_ "Keyboard Services"
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

interrupt 0x20 = do
    trace_ "halt"
    throwError Halt

interrupt 0x21 = do
    trace_ "DOS rutine"
    v <- use ah
    case v of
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
                let f = map (chr . fromIntegral) $ takeWhile (/=0) fname
                trace_ $ "File: " ++ show f
                let s = unsafePerformIO $ BS.readFile $ "../original/" ++ map toUpper f
    --            ax .= 02  -- File not found
                handle <- imMax <$> use files
                files %= IM.insert handle (f, s, 0)
                ax' .= "file handle" @: fromIntegral handle
                carryF .= False

        0x3e -> do
            trace_ "Close file"
            handle <- fromIntegral <$> use bx
            x <- IM.lookup handle <$> use files
            case x of
              Just (fn, _, _) -> do
                trace_ $ "file: " ++ fn
                files %= IM.delete handle
                carryF .= False

        0x3f -> do
            trace_ "Read from file or device"
            handle <- fromIntegral <$> use bx
            num <- fromIntegral <$> use cx
            loc <- use $ addressOf Nothing $ memIndex RDX
            s <- BS.take num . (\(fn, s, p) -> BS.drop p s) . (IM.! handle) <$> use files
            let len = BS.length s
            files %= flip IM.adjust handle (\(fn, s, p) -> (fn, s, p+len))
            heap . bytesAt loc len .= BS.unpack s
            ax' .= "length" @: fromIntegral len
            carryF .= False

        0x42 -> do
            trace_ "Set Current File Position"
            handle <- fromIntegral <$> use bx
            mode <- use al
            pos <- fromIntegral . asSigned <$> use (uComb cx dx . combine)
            files %= (flip IM.adjust handle $ \(fn, s, p) -> case mode of
                0 -> (fn, s, pos)
                1 -> (fn, s, p + pos)
                2 -> (fn, s, BS.length s - pos)
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
                trace_ "Get Device Information" 
                handle <- use bx
                case handle of
                  4 -> dx .= 0x2804        --  0010 1000 00 000100   no D: drive
                  3 -> dx .= 0x2883        --  0010 1000 00 000011   no C: drive
                  2 -> dx .= 0x2802        --  0010 1000 00 000011    B: drive
                  1 -> dx .= 0x2801        --  0010 1000 00 000011    A: drive
                  0 -> dx .= 0x2800        --  0010 1000 00 000011    default drive
            carryF .= False

        0x48 -> do
            trace_ "Allocate Memory"
            memory_paragraphs_requested <- use bx
            x <- zoom heap $ extendMem $ memory_paragraphs_requested + 1
            ax' .= "segment address of allocated memory block" @: x + 1 -- (MCB + 1para)
            carryF .= False

        0x4a -> do
            trace_ "Modify allocated memory blocks"
            new_requested_block_size_in_paragraphs <- use bx
            segment_of_the_block <- use es      -- (MCB + 1para)
            Overlay ram rom <- use heap
--            throwError $ Err $ showRom rom
            heap .= Overlay ram (extendRom' (segment_of_the_block ^. paragraph) (new_requested_block_size_in_paragraphs ^. paragraph) rom)
            carryF .= False      -- unlimited memory available

        0x4e -> do
            trace_ "Find First Matching File"
            attribute_used_during_search <- use cx
            addr <- use $ addressOf Nothing $ memIndex RDX
            fname <- use $ heap . bytesAt addr 20
            let f = map (chr . fromIntegral) $ takeWhile (/=0) fname
            trace_ $ "File: " ++ show f
            ad <- use dta
--            throwError Halt

            let s = unsafePerformIO $ BS.readFile $ "../original/" ++ map toUpper f
--            ax .= 02  -- File not found
            heap . bytesAt 0 0x1a .= repeat (error "undefined byte")
            heap . dwordAt (ad + 0x1a) .= fromIntegral (BS.length s)
            heap . bytesAt (ad + 0x1e) 13 .= map (fromIntegral . ord) f ++ [0]
            carryF .= False

        0x62 -> do
            trace_ "Get PSP address (DOS 3.x)"
            bx' .= "segment address of current process" @: 0x100 - 0x10  -- hack!!!
            carryF .= False

        _    -> throwError $ Err $ "dos function #" ++ showHex' 2 v

interrupt 0x33 = do
    trace_ "Mouse Services"
    v <- use ax
    case v of
        0x00 -> do
            trace_ "Mouse Reset/Get Mouse Installed Flag"
            ax' .= "mouse driver not installed" @: 0x0000
            bx' .= "number of buttons" @: 0

interrupt v = throwError $ Err $ "interrupt #" ++ showHex' 2 v
{-
interrupt v = do
    use flags >>= push
    use cs >>= push
    use ip >>= push
    interruptF .= False
    case v of
        _ -> throwError $ Err $ "interrupt #" ++ show v
-}

----------------------------------------------

infixl 9 @.
m @. i = push_ i >> m

class PushVal a where
    push_ :: a -> Machine ()
instance PushVal Word16 where
    push_ = push . noAnn

----------------------------------------------

interruptTable = replicate 1024 0

prelude = interruptTable
    ++ take 16 ([0xf4] ++ repeat 0) -- halt instruction

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
programSegmentPrefix envseg endseg args = flip execState (toRom $ replicate 0x100 (error "psp uninitialized byte")) $ do

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

envvars = BS.concat (map (`BS.append` "\NUL") ["PATH="]) `BS.append` "\NUL"

loadExe :: IM.IntMap BS.ByteString -> Word16 -> BS.ByteString -> MachineState
loadExe labs loadSegment gameExe = flip execState emptyState $ do
    heap .= Overlay (toRom [])
        (Split 0xa0000
            (toRom $ concat
                [ prelude
                , BS.unpack envvars
                , replicate (loadSegment ^. paragraph - length prelude - BS.length envvars - pspSize) 0
                , fromRom 0 256 $ programSegmentPrefix (length prelude ^. from paragraph) endseg ""
                , BS.unpack $ relocate relocationTable loadSegment $ BS.drop headerSize gameExe
                , replicate (additionalMemoryAllocated ^. paragraph) (fromIntegral $ ord '?')
                ])
            (Zeros 0xa0000 0x10000)
        )
    ss .= ssInit + loadSegment
    sp .= spInit
    cs .= csInit + loadSegment
    ip .= ipInit
    ds .= pspSegment
    es .= pspSegment
    labels .= IM.fromDistinctAscList (map ((+ reladd) *** id) $ IM.toList labs)
    clearHist
  where
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

