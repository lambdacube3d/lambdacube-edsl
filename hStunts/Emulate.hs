{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
import Hdis86
import Hdis86.Types
import Hdis86.Incremental
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Data.Bits
import Data.Char
import Data.Maybe
import qualified Data.IntMap as IM
import Numeric
import Control.Applicative hiding (Const)
import Control.Arrow
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))
import GHC.Int (Int64)

----------------------------------------------

gameExe = unsafePerformIO $ BS.drop headerSize <$> BS.readFile "original/game.exe"
headerSize = 0x2890
dataSegment = 0x2b77

segments :: V.Vector Word16
segments = V.fromList [0, 0, 0, 0, 0,  0, 0x15f2{-?-}, 0, 0, 0,  0, 0x1ea0{-?-}, 0x1ea2]

symbols =
    [ (,) "sin"               (12, 0x3cbe)    -- 0x24f6e - hs
    , (,) "cos"               (12, 0x3d0c)
    , (,) "polarAngle"        (12, 0x2e)      -- 0x212de - hs
    , (,) "polarRadius2D"     (12, 0x1696)
    , (,) "polarRadius3D"     (11, 0x8)       -- 0x21298 - hs
    , (,) "rectComparePoint"  ( 6, 0xe) -- 0x187be - hs   -- x * 16 + y = 0x15ee2
    ]

disasmConfig = Config Intel Mode16 SyntaxIntel 0

tests = do
    quickCheck $ \i -> eval (call "sin" @. i) == sin_fast i
    quickCheck $ \i -> eval (call "cos" @. i) == cos_fast i
    quickCheck $ \i j -> eval (call "polarAngle" @. j @. i) == fromIntegral (polarAngle (fromIntegral' i) (fromIntegral' j))
    quickCheck $ \i j -> eval (call "polarRadius2D" @. j @. i) == fromIntegral (polarRadius2D (fromIntegral' i) (fromIntegral' j))
    q3d

q3d = quickCheck $ \i_ j_ k_ -> let
        i = extend8_16 i_
        j = extend8_16 j_
        k = extend8_16 k_
        v = V.fromList [i,j,k] :: Vect
    in {-polarRadius2D (fromIntegral' i) (fromIntegral' j) == 0x8000
     || -} eval (call "polarRadius3D" @. v) == fromIntegral (polarRadius3D $ V.map fromIntegral' v)

qrp = quickCheck $ \i j ->
        eval (call "rectComparePoint" @. (V.fromList [i,j] :: Vect)) == 1

----------------------------------------------

data State = State
    { cs :: !Word16          -- code segment
    , ss :: !Word16          -- stack segment
    , ds :: !Word16          -- data segment
    , es :: !Word16          -- extra segment

    , ip :: !Word16          -- instruction pointer
    , sp :: !Word16          -- stack pointer
    , bp :: !Word16

    , ax :: !Word16
    , bx :: !Word16
    , cx :: !Word16
    , dx :: !Word16
    , si :: !Word16
    , di :: !Word16

    , flags :: !Word16

    , code  :: !ByteString    -- code segments
    , heap  :: !(IM.IntMap Word8)
    , stack :: !(V.Vector Word16)
    }

initState :: (Int, Word16) -> State
initState (seg, ip) = State
    { stack = V.replicate (2^10) 0
    , heap  = IM.empty
    , code  = gameExe

    , ss = 0
    , cs = segments V.! seg
    , ds = dataSegment
    , es = 0

    , ip = ip
    , bp = 0
    , sp = 2^10

    , ax = 0
    , bx = 0
    , cx = 0
    , dx = 0
    , si = 0
    , di = 0

    , flags = 0x7202
    }

call :: String -> State
call x = initState $ head [addr | (x',addr) <- symbols, x == x']

stateDiff :: State -> State -> String
stateDiff s v = unwords $ catMaybes $ 
       zipWith3 diff names1 (f1 s) (f1 v)
    ++ zipWith3 diff names2 (f2 s) (f2 v)
    ++ zipWith3 diff names3 (f3 s) (f3 v)
  where
    diff f x y | x == y = Nothing
               | otherwise = Just $ f y
    names1 = map (\n y -> n ++ "=" ++ showHex' 4 y) ["AX","BX","CX","DX","SI","DI","DS","ES","BP","CS","SS","SP"]
    names2 = map (\n y -> y) (replicate 4 ())
    names3 = map (\n y -> "S[" ++ showHex' 4 n ++ "]=" ++ showHex' 4 y) [0,2..]
    f1 State{..} = [ax,bx,cx,dx,si,di,ds,es,bp,cs,ss,sp]
    f2 State{..} = [overflowF,signF,zeroF,carryF]
      where
        getFlag c i = [if (flags `shiftR` i) .&. 1 == 0 then toLower c else c]
        carryF    = getFlag 'C' 0
        zeroF     = getFlag 'Z' 6
        signF     = getFlag 'S' 7
        overflowF = getFlag 'O' 11
    f3 State{..} = V.toList stack

take' n l = case splitAt n l of
    (xs, []) -> xs
    (xs, _) -> xs ++ ["..."]

showHex' i x = replicate (i - length s) '0' ++ s where s = showHex x ""

showReg "" _ = "       "
showReg n r = n ++ ":" ++ showHex' 4 r

pad i s = s ++ take (i - length s) (repeat ' ')

numOfDisasmLines = 30

instance Show State where
    show s@State{..} = unlines
        [ "  Flags: " ++ overflowF ++ signF ++ zeroF ++ carryF
        , ("  "++) $ unwords $ zipWith showReg ["AX","BX","CX","DX","SI","DI"] [ax,bx,cx,dx]
        , ("  "++) $ unwords $ zipWith showReg ["SI","DI","IP","SP","BP"] [si,di,ip,sp,bp]
        , ("  "++) $ unwords $ zipWith showReg ["DS","ES","CS","SS"] [ds,es,cs,ss]
        , ("Stack: " ++) $ unwords $ take' 10 . zipWith (\i _ -> showHex' 4 i) [sp,sp+2..] $ V.toList $ V.drop (fromIntegral sp `div` 2) stack
        , ("       " ++) $ unwords $ take' 10 . map (showHex' 4) $ V.toList $ V.drop (fromIntegral sp `div` 2) stack
        , "Code: "
        , unlines $ map (take 149 . ("  " ++)) . take' numOfDisasmLines $ coode True s
        ]
      where
        getFlag c i = [if (flags `shiftR` i) .&. 1 == 0 then toLower c else c]
        carryF    = getFlag 'C' 0
        zeroF     = getFlag 'Z' 6
        signF     = getFlag 'S' 7
        overflowF = getFlag 'O' 11

coode sc s@State{..} = zipWith (\x (b, y) -> (if sc then pad 14 (map toUpper $ mdHex x) ++ " " else "")
    ++ (if b || sc then pad 25 (mdAssembly x) ++ " " else "") ++ y) (disassembleMetadata disasmConfig $ BS.drop ips code) (sts sc s)
 where
        ips = (fromIntegral cs `shiftL` 4) + fromIntegral ip :: Int

sts :: Bool -> State -> [(Bool, String)]
sts sc s
    | cs s == cs s' && ip s' == ip s + (fromIntegral $ mdLength i) = (True, stateDiff s s'): sts sc s'
    | inOpcode (mdInst i) == Icall = (True, stateDiff s s''): sts sc s''
    | otherwise = (True, "JUMP"): map ((,) False . ("|" ++)) (coode False s')
  where
    (i, s') = step_ s
    s'' = stepthrough s

stepthrough = step 1 . findRet 0 . step 1

findRet n st
    | inOpcode (mdInst op) == Iretf && n == 0 = st
    | inOpcode (mdInst op) == Iretf = findRet (n-1) st'
    | inOpcode (mdInst op) == Icall = findRet (n+1) st'
    | otherwise = findRet n st'
  where
    (op, st') = step_ st

infixl 9 @.

class PushVal a where
    (@.) :: State -> a -> State
instance PushVal Word16 where
    s@State{..} @. x = s { stack = writeVec (fromIntegral $ (sp + 2) `div` 2) x stack, sp = sp - 2 }
instance PushVal (V.Vector Word16) where
    s@State{..} @. v = s { stack = writeVec (fromIntegral $ (sp + 2) `div` 2) x stack, sp = sp - 2, heap = h `IM.union` heap }
        where
        x = 0xf000
        h = IM.fromList $ zip [(fromIntegral ds `shiftL` 4) + fromIntegral x ..] $ concatMap spl $ V.toList v

spl :: Word16 -> [Word8]
spl w = [fromIntegral w, fromIntegral (w `shiftR` 8)]

eval s = ax $ findRet 0 $ s

step 0 s = s
step n s = step (n-1) $ snd $ step_ s

(&) :: a -> (a -> b) -> b
x & f = f x

infixl 0 &

push x s@State{..} = s { sp = sp', stack = writeVec (fromIntegral $ sp' `shiftR` 1) x stack }
    where
        sp' = sp - 2
pop s@State{..} = (stack V.! fromIntegral (sp `shiftR` 1), s { sp = sp' })
    where
        sp' = sp + 2

quotRem' :: Integral a => a -> a -> (a, a)
quotRem' a 0 = (0, 0)
quotRem' a b = quotRem a b

step_ :: State -> (Metadata, State)
step_ s@State{..} = (md, exec (mdInst md) $ s { ip = ip' })
  where
    Just (md, rem) = disassembleOne disasmConfig $ BS.drop ips code

    ips = (fromIntegral cs `shiftL` 4) + fromIntegral ip :: Int
    ip' = ip + fromIntegral (mdLength md)

    exec i s@State{..} = case i of
        Inst [] Ipush [x] -> push (fetch Nothing x) s
        Inst [] Ipop [x] -> update x y s'
            where
                (y,s') = pop s
        Inst [] Imov [dest, src] -> update dest (fetch Nothing src) s
        Inst [Seg seg] Imov [dest, src] -> update dest (fetch (Just seg) src) s
        Inst [] Iles [dest, Mem src] -> updateSegment ES (fetchMem' $ addAddr 2 ad) $ update dest (fetchMem' ad) s
            where ad = compAddr src
        Inst [] Ixchg [dest, src] -> update src (fetch Nothing dest) $ update dest (fetch Nothing src) s
        Inst [] Idiv [src] -> update (Reg $ Reg16 RAX) (fromIntegral d) $ update (Reg $ Reg16 RDX) (fromIntegral m) s
            where
                (d, m) = (((fromIntegral dx :: Word32) `shiftL` 16) .|. fromIntegral ax) `quotRem'` fromIntegral (fetch Nothing src)
        Inst [] Imul [src] -> update (Reg $ Reg16 RAX) low $ update (Reg $ Reg16 RDX) high s
            where
                high = fromIntegral $ r `shiftR` 16 
                low = fromIntegral $ r
                r = fromIntegral ax * fromIntegral (fetch Nothing src) :: Int
        Inst [] Ixor [dest, src] -> twoOp Nothing True xor xor dest src
        Inst [] Ior  [dest, src] -> twoOp Nothing True (.|.) (.|.) dest src
        Inst [] Iand [dest, src] -> twoOp Nothing True (.&.) (.&.) dest src
        Inst [] Ishl [dest, src] -> shiftOp shiftL' dest src
        Inst [] Isar [dest, src] -> shiftOp shiftAR dest src
        Inst [] Ircr [dest, src] -> shiftOp shiftRCR dest src
        Inst [] Ineg [dest]      -> update dest (- fetch Nothing dest) s
        Inst [] Icwd []          -> update (Reg $ Reg16 RDX) (if ax >= 0x8000 then 0xffff else 0) s
        Inst [] Icbw []          -> update (Reg $ Reg16 RAX) (extend8_16 $ fromIntegral ax) s
        Inst [] Iadd [dest, src] -> twoOp Nothing True (+) (+) dest src
        Inst [] Iadc [dest, src] -> twoOp Nothing True adc adc dest src
        Inst [] Isbb [dest, src] -> twoOp Nothing True sbb sbb dest src
        Inst [] Isub [dest, src] -> twoOp Nothing True (-) (-) dest src
        Inst [Seg seg] Isub [dest, src] -> twoOp (Just seg) True (-) (-) dest src
        Inst [] Icmp [dest, src] -> twoOp Nothing False (-) (-) dest src
        Inst [Seg seg] Ijmp [Mem (Memory size r RegNone 0 (Immediate Bits16 v))] ->
            s { ip = fetchMem size seg (fromIntegral (fetch Nothing $ Reg r) + fromIntegral v) }
        Inst [] Ijmp [Jump (Immediate Bits8 v)] ->
            s { ip = ip + extend8_16 (fromIntegral v) }
        Inst [] Ijge [Jump (Immediate Bits8 v)] ->
            if signF == overflowF then s { ip = ip + extend8_16 (fromIntegral v) } else s
        Inst [] Ijg [Jump (Immediate Bits8 v)] ->
            if signF == overflowF && zeroF == 0 then s { ip = ip + extend8_16 (fromIntegral v) } else s
        Inst [] Ijl [Jump (Immediate Bits8 v)] ->
            if signF /= overflowF then s { ip = ip + extend8_16 (fromIntegral v) } else s
        Inst [] Ijle [Jump (Immediate Bits8 v)] ->
            if signF /= overflowF || zeroF == 1 then s { ip = ip + extend8_16 (fromIntegral v) } else s
        Inst [] Ijz [Jump (Immediate Bits8 v)] ->
            if zeroF == 1 then s { ip = ip + extend8_16 (fromIntegral v) } else s
        Inst [] Ijnz [Jump (Immediate Bits8 v)] ->
            if zeroF == 0 then s { ip = ip + extend8_16 (fromIntegral v) } else s
        Inst [] Icall [Ptr (Pointer seg (Immediate Bits16 v))] ->
            push ip $ push cs $ s { cs = fromIntegral seg, ip = fromIntegral v }
        Inst [] Iretf [] -> s'' { cs = cs, ip = ip }
          where
            (ip, s') = pop s
            (cs, s'') = pop s'
        Inst [] Iretf [Imm i] -> s'' { cs = cs, ip = ip, sp = sp + imm i }
          where
            (ip, s') = pop s
            (cs, s'') = pop s'
        i -> error $ "step: " ++ show i
      where
        getFlag i = (flags `shiftR` i) .&. 1
        carryF    = getFlag 0
        zeroF     = getFlag 6
        signF     = getFlag 7
        overflowF = getFlag 11

        adc a b = a + b + fromIntegral carryF
        sbb a b = a - b - fromIntegral carryF

        fetchMem' (a,b,c) = fetchMem a b c

        writeMem (Bits16, SS, i) v s@State{..} = s { stack = writeVec (fromIntegral $ i `shiftR` 1) v stack }

        fetchMem Bits16 SS i = stack V.! (i `shiftR` 1)
        fetchMem Bits16 seg i = fromIntegral low + (fromIntegral high `shiftL` 8)
          where
            addr = fetchS seg + i
            low = fromMaybe low_ $ IM.lookup addr heap
            high = fromMaybe high_ $ IM.lookup (addr + 1) heap
            (low_: high_: _) = BS.unpack (BS.drop addr code) ++ repeat 0
        fetchMem Bits8 seg i = fromIntegral low
          where
            addr = fetchS seg + i
            low = fromMaybe low_ $ IM.lookup addr heap
            (low_: _) = BS.unpack (BS.drop addr code) ++ repeat 0

        fetchS x = case x of
            CS  -> fromIntegral cs `shiftL` 4
            ES  -> fromIntegral es `shiftL` 4
            DS  -> fromIntegral ds `shiftL` 4

        imm :: Immediate Word64 -> Word16
        imm (Immediate Bits8 v) = fromIntegral v
        imm (Immediate Bits16 v) = fromIntegral v

        imm' :: Immediate Int64 -> Int
        imm' (Immediate Bits0 _) = 0
        imm' (Immediate Bits8 v) = fromIntegral v
        imm' (Immediate Bits16 v) = fromIntegral v

        addAddr d (a,b,c) = (a,b,c+d)
        compAddr (Memory Bits0 (Reg16 RBP) RegNone 0 i) = (Bits16, SS, fromIntegral bp + imm' i)
        compAddr (Memory Bits16 (Reg16 RBP) RegNone 0 i) = (Bits16, SS, fromIntegral bp + imm' i)
        compAddr m = error $ "compAddr: " ++ show m

        fetch :: Maybe Segment -> Operand -> Word16
        fetch ms x = case x of
            Reg (Reg16 r) -> reg r
            Reg (Reg8 r L) -> reg r .&. 0xff
            Reg (Reg8 r H) -> reg r `shiftR` 8
            Mem (Memory s (Reg16 RBP) RegNone 0 i)
                -> fetchMem s (ff SS) $ fromIntegral bp + imm' i
            Mem (Memory s (Reg16 RBX) RegNone 0 i)
                -> fetchMem s (ff DS) $ fromIntegral bx + imm' i
            Mem (Memory s (Reg16 RSI) RegNone 0 i)
                -> fetchMem s (ff DS) $ fromIntegral si + imm' i
            Mem (Memory s RegNone RegNone 0 i)
                -> fetchMem s (ff DS) $ imm' i
            Imm i -> imm i
            Const (Immediate Bits0 0) -> 1 -- !!!
            _ -> error $ "fetch: " ++ show x
          where
            ff x = fromMaybe x ms
            reg r = case r of
                RBP -> bp
                RSP -> sp
                RAX -> ax
                RBX -> bx
                RCX -> cx
                RDX -> dx
                RSI -> si
                RDI -> di

        updateFlags :: Bool -> Bool -> Word16 -> State -> State
        updateFlags car ovf x s
            = s { flags = (flags .&. 0xf73e)
                      .|. (if car then 1 else 0)      -- carry flag
                      .|. ((x .&. 0x8000) `shiftR` 4)      -- sign flag
                      .|. (if x == 0 then 2^6 else 0)      -- zero flag
                      .|. (if ovf then 2^11 else 0)        -- overflow flag
                }

        updateFlags' car s
            = s { flags = (flags .&. 0xfffe)
                      .|. (if car then 1 else 0)      -- carry flag
                }

        shiftOp :: (forall a . (Integral a, FiniteBits a) => a -> Int -> (Bool, a)) -> Operand -> Operand -> State
        shiftOp op x y = update x r $ updateFlags' carry s
          where
            a = fetch Nothing x
            b = fromIntegral $ fetch Nothing y :: Int
            (carry, r) = case size x of
                16 -> op a b
                8 -> id *** fromIntegral $ op (fromIntegral a :: Word8) b

        twoOp :: Maybe Segment -> Bool -> (forall a . (Integral a, FiniteBits a) => a -> a -> a) -> (Int -> Int -> Int) -> Operand -> Operand -> State
        twoOp ms store op op' x y = (if store then update x r else id) $ updateFlags carry overflow r s
          where
            a = fetch ms x
            b = fetch ms y
            r = case size x of
                16 -> op a b
                8 -> fromIntegral $ op (fromIntegral a :: Word8) (fromIntegral b)
            r' = fromIntegral $ op' (fromIntegral' a) (fromIntegral' b)
            r'' = fromIntegral $ op' (fromIntegral a) (fromIntegral b)
            overflow = r /= r'
            carry = r /= r''

        size (Reg (Reg16 _)) = 16
        size (Reg (Reg8 _ _)) = 8
        size (Mem (Memory Bits16 _ _ _ _)) = 16
        size s = error $ "size: " ++ show s

        updateSegment ES x s@State{..} = s { es = x }

        update :: Operand -> Word16 -> State -> State
        update x y s@State{..} = case x of
            Reg (Reg16 r) -> case r of
                RBP -> s { bp = y }
                RSP -> s { sp = y }
                RAX -> s { ax = y }
                RBX -> s { bx = y }
                RCX -> s { cx = y }
                RDX -> s { dx = y }
                RSI -> s { si = y }
                RDI -> s { di = y }
            Reg (Reg8 r L) -> case r of
                RAX -> s { ax = (ax .&. 0xff00) .|. y }
                RBX -> s { bx = (bx .&. 0xff00) .|. y }
            Reg (Reg8 r H) -> case r of
                RAX -> s { ax = (ax .&. 0xff) .|. (y `shiftL` 8) }
                RBX -> s { bx = (bx .&. 0xff) .|. (y `shiftL` 8) }
            Mem m -> writeMem (compAddr m) y s
            x -> error $ "update: " ++ show x

        shiftRCR :: FiniteBits a => a -> Int -> (Bool, a)
        shiftRCR x i = (testBit x 0, setBit' r s $ carryF == 1)
          where
            r = x `shiftR` i
            s = finiteBitSize x - 1

setBit' :: Bits a => a -> Int -> Bool -> a
setBit' x i True = setBit x i
setBit' x i False = clearBit x i

shiftL' :: FiniteBits a => a -> Int -> (Bool, a)
shiftL' x i = (testBit x s, x `shiftL` i)
  where
    s = finiteBitSize x - 1

shiftAR :: FiniteBits a => a -> Int -> (Bool, a)
shiftAR x i = (testBit x 0, if testBit x s then setBit r s else r)
  where
    r = x `shiftR` i
    s = finiteBitSize x - 1

fromIntegral' :: Word16 -> Int
fromIntegral' v = if v >= 0x8000 then fromIntegral v - 0x10000 else fromIntegral v

writeVec :: Int -> a -> V.Vector a -> V.Vector a
writeVec i a = V.modify $ \v -> MV.write v i a

extend8_16 :: Word8 -> Word16
extend8_16 v = if v >= 0x80 then 0xff00 .|. fromIntegral v else fromIntegral v

------------------------

sintab :: V.Vector Word16
sintab = V.fromList [0, 101, 201, 302, 402, 503, 603, 704, 804, 904, 1005, 1105, 1205, 1306, 1406, 1506, 1606, 1706, 1806, 1906, 2006, 2105, 2205, 2305, 2404, 2503, 2603, 2702, 2801, 2900, 2999, 3098, 3196, 3295, 3393, 3492, 3590, 3688, 3786, 3883, 3981, 4078, 4176, 4273, 4370, 4467, 4563, 4660, 4756, 4852, 4948, 5044, 5139, 5235, 5330, 5425, 5520, 5614, 5708, 5803, 5897, 5990, 6084, 6177, 6270, 6363, 6455, 6547, 6639, 6731, 6823, 6914, 7005, 7096, 7186, 7276, 7366, 7456, 7545, 7635, 7723, 7812, 7900, 7988, 8076, 8163, 8250, 8337, 8423, 8509, 8595, 8680, 8765, 8850, 8935, 9019, 9102, 9186, 9269, 9352, 9434, 9516, 9598, 9679, 9760, 9841, 9921, 10001, 10080, 10159, 10238, 10316, 10394, 10471, 10549, 10625, 10702, 10778, 10853, 10928, 11003, 11077, 11151, 11224, 11297, 11370, 11442, 11514, 11585, 11656, 11727, 11797, 11866, 11935, 12004, 12072, 12140, 12207, 12274, 12340, 12406, 12472, 12537, 12601, 12665, 12729, 12792, 12854, 12916, 12978, 13039, 13100, 13160, 13219, 13279, 13337, 13395, 13453, 13510, 13567, 13623, 13678, 13733, 13788, 13842, 13896, 13949, 14001, 14053, 14104, 14155, 14206, 14256, 14305, 14354, 14402, 14449, 14497, 14543, 14589, 14635, 14680, 14724, 14768, 14811, 14854, 14896, 14937, 14978, 15019, 15059, 15098, 15137, 15175, 15213, 15250, 15286, 15322, 15357, 15392, 15426, 15460, 15493, 15525, 15557, 15588, 15619, 15649, 15679, 15707, 15736, 15763, 15791, 15817, 15843, 15868, 15893, 15917, 15941, 15964, 15986, 16008, 16029, 16049, 16069, 16088, 16107, 16125, 16143, 16160, 16176, 16192, 16207, 16221, 16235, 16248, 16261, 16273, 16284, 16295, 16305, 16315, 16324, 16332, 16340, 16347, 16353, 16359, 16364, 16369, 16373, 16376, 16379, 16381, 16383, 16384, 16384]

sin_fast, cos_fast :: Word16 -> Word16
sin_fast s = case (s `shiftR` 8) .&. 3 of
    0 -> sintab V.! c
    1 -> sintab V.! (0x100 - c)
    2 -> - sintab V.! c
    3 -> - sintab V.! (0x100 - c)
  where
    c = fromIntegral (s .&. 0xFF)

cos_fast = sin_fast . (+ 0x100)

type Matrix = V.Vector Word16  -- 11 21 31 12 22 32 13 23 33
                               -- 0  1  2  3  4  5  6  7  8
type Vect = V.Vector Word16

mat_mul_vector :: Vect -> Matrix -> Vect
mat_mul_vector invec m = V.fromList
    [ invec V.! 0 * m V.! 0 + invec V.! 1 * m V.! 3 + invec V.! 2 * m V.! 6
    , invec V.! 0 * m V.! 1 + invec V.! 1 * m V.! 4 + invec V.! 2 * m V.! 7
    , invec V.! 0 * m V.! 2 + invec V.! 1 * m V.! 5 + invec V.! 2 * m V.! 8
    ]

polarAngle :: Int -> Int -> Int
polarAngle 0 0 = 0 --undefined
polarAngle z y = case (z < 0, y < 0, z_ > y_) of
    (False, False, False) ->  result
    (False, False,  True) -> -result + 0x100
    (False,  True, False) -> -result + 0x200
    (False,  True,  True) ->  result + 0x100
    ( True, False, False) -> -result
    ( True, False,  True) ->  result - 0x100
    ( True,  True, False) ->  result - 0x200
    ( True,  True,  True) -> -(result + 0x100)
  where
    y_ = abs y
    z_ = abs z
    (z', y') = if z_ > y_ then (y_, z_) else (z_, y_)

    result
        | z == y = 0x80
        | otherwise = atantable V.! index

    index = round ((fromIntegral z' `shiftL` 16) `div` y') `shiftR` 8

    round x | (x .&. 0xFF) >= 0x80 = x + 0x100
            | otherwise            = x

atantable :: V.Vector Int
atantable = V.fromList [0, 1, 1, 2, 3, 3, 4, 4, 5, 6, 6, 7, 8, 8, 9, 10, 10, 11, 11, 12, 13, 13, 14, 15, 15, 16, 16, 17, 18, 18, 19, 20, 20, 21, 22, 22, 23, 23, 24, 25, 25, 26, 27, 27, 28, 28, 29, 30, 30, 31, 31, 32, 33, 33, 34, 34, 35, 36, 36, 37, 38, 38, 39, 39, 40, 41, 41, 42, 42, 43, 44, 44, 45, 45, 46, 46, 47, 48, 48, 49, 49, 50, 51, 51, 52, 52, 53, 53, 54, 55, 55, 56, 56, 57, 57, 58, 58, 59, 60, 60, 61, 61, 62, 62, 63, 63, 64, 65, 65, 66, 66, 67, 67, 68, 68, 69, 69, 70, 70, 71, 71, 72, 72, 73, 74, 74, 75, 75, 76, 76, 77, 77, 78, 78, 79, 79, 80, 80, 81, 81, 82, 82, 83, 83, 84, 84, 84, 85, 85, 86, 86, 87, 87, 88, 88, 89, 89, 90, 90, 91, 91, 91, 92, 92, 93, 93, 94, 94, 95, 95, 96, 96, 96, 97, 97, 98, 98, 99, 99, 99, 100, 100, 101, 101, 102, 102, 102, 103, 103, 104, 104, 104, 105, 105, 106, 106, 106, 107, 107, 108, 108, 108, 109, 109, 110, 110, 110, 111, 111, 112, 112, 112, 113, 113, 113, 114, 114, 115, 115, 115, 116, 116, 116, 117, 117, 118, 118, 118, 119, 119, 119, 120, 120, 120, 121, 121, 121, 122, 122, 122, 123, 123, 123, 124, 124, 124, 125, 125, 125, 126, 126, 126, 127, 127, 127, 128, 128]

polarRadius2D :: Int -> Int -> Int
polarRadius2D z y = g $ f $ abs $ polarAngle z y
  where
    f r | r >= 0x100 = -(r - 0x200)
        | otherwise = r

    g r | r <= 0x80 = (abs y `shiftL` 14) `div` fromIntegral' (cos_fast $ fromIntegral r)
        | otherwise = (abs z `shiftL` 14) `div` fromIntegral' (sin_fast $ fromIntegral r)

polarRadius3D :: V.Vector Int -> Int
polarRadius3D vec = polarRadius2D (polarRadius2D (vec V.! 0) (vec V.! 1)) (vec V.! 2)

