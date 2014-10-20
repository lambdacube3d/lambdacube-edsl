{-# LANGUAGE RecordWildCards #-}
import Hdis86
import Hdis86.Types
import Hdis86.Incremental
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Data.Bits
import Data.Char
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))


orig = unsafePerformIO $ BS.readFile "original/uncomp.exe"

start ip = State
    { stack = V.replicate 2000 0
    , code  = orig
    , heap  = V.replicate 2000 0

    , ip = ip
    , bp = 0
    , sp = 1000

    , ss = 0
    , cs = 0x1ec2
    , ds = 0
    , es = 0

    , ax = 0
    , bx = 0
    , cx = 0
    , dx = 0
    , si = 0
    , di = 0

    , flags = 0x7202
    }

emulFun ip x = ax $ f $ s { stack = writeVec 502 x $ stack s }
  where
    s = start ip
    f st | inOpcode (mdInst op) == Iretf = st
         | otherwise = f st'
      where
        (op, st') = step st

emulFun2 ip x y = ax $ f $ s { stack = writeVec 504 y $ writeVec 502 x $ stack s }
  where
    s = start ip
    f st | inOpcode (mdInst op) == Iretf = st
         | otherwise = f st'
      where
        (op, st') = step st

emulFun2' ip x y n = stepN n $ s { stack = writeVec 504 y $ writeVec 502 x $ stack s }
  where
    s = start ip

emulFun' ip x n = stepN n $ s { stack = writeVec 502 x $ stack s }
  where
    s = start ip

initFun ip x = s { stack = writeVec 502 x $ stack s }
  where
    s = start ip

sinEmul = emulFun 0x3cbe
cosEmul = emulFun 0x3d0c
polarAngleEmul = emulFun2 0x2e
polarAngleEmul' = emulFun2' 0x2e

sinOK = quickCheck $ \i -> sinEmul i == sin_fast i
cosOK = quickCheck $ \i -> cosEmul i == cos_fast i

-- start  1CE62

-- 57745   seg012      0x1EC2A  ?  0x1EC20
-- 66412   LookupSine   0x228de
-- ugrótábla: 0x228f2     -- 0x3cd2
-- 72606   set013
-- g_pwQuarterSineTab      :4c84      0x2ec9e
-- adat szegmens kezdete: 0x2a01a  ?

stepN 1 s = step s
stepN n s = stepN (n-1) $ snd $ step s

showFlags State{..} = overflowF ++ signF ++ zeroF ++ carryF
  where
        getFlag c i = [if (flags `shiftR` i) .&. 1 == 0 then toLower c else c]
        carryF    = getFlag 'C' 0
        zeroF     = getFlag 'Z' 6
        signF     = getFlag 'S' 7
        overflowF = getFlag 'O' 11

step :: State -> (Metadata, State)
step s@State{..} = (md, exec (mdInst md) $ s { ip = ip' })
  where
    Just (md, rem) = disassembleOne (Config Intel Mode16 SyntaxIntel 0) $ BS.drop ips code

    ips = (fromIntegral cs `shiftL` 4) + fromIntegral ip :: Int
    ip' = ip + fromIntegral (mdLength md)

    exec i s@State{..} = case i of
        Inst [] Ipush [x] -> s { sp = sp', stack = writeVec (fromIntegral $ sp' `shiftR` 1) (fetch x) stack }
            where
                sp' = sp - 2
        Inst [] Ipop [x] -> update x (stack V.! fromIntegral (sp `shiftR` 1)) $ s { sp = sp' }
            where
                sp' = sp + 2
        Inst [] Imov [dest, src] -> update dest (fetch src) s
        Inst [] Ixchg [dest, src] -> update src (fetch dest) $ update dest (fetch src) s
        Inst [] Idiv [src] -> update (Reg $ Reg16 RAX) (fromIntegral d) $ update (Reg $ Reg16 RDX) (fromIntegral m) s
            where
                (d, m) = (((fromIntegral dx :: Word32) `shiftL` 16) .|. fromIntegral ax) `quotRem` fromIntegral (fetch src)
        Inst [] Ixor [dest, src] -> twoOp True xor xor dest src
        Inst [] Ior  [dest, src] -> twoOp True (.|.) (.|.) dest src --update dest (fetch dest .|. fetch src) s
        Inst [] Iand [dest, src] -> twoOp True (.&.) (.&.) dest src --update dest (fetch dest .&. fetch src) s
        Inst [] Ishl [dest, src] -> twoOp True (\a b -> a `shiftL` fromIntegral b) (\a b -> a `shiftL` fromIntegral b) dest src -- update dest (fetch dest `shiftL` fromIntegral (fetch src)) s
        Inst [] Ineg [dest] -> update dest (- fetch dest) s
        Inst [] Iadd [dest, src] -> twoOp True (+) (+) dest src --update dest (fetch dest + fetch src) s
        Inst [] Iadc [dest, src] -> twoOp True adc adc dest src --update dest (fetch dest + fetch src) s
        Inst [] Isub [dest, src] -> twoOp True (-) (-) dest src --update dest (fetch dest - fetch src) s
        Inst [] Icmp [dest, src] -> twoOp False (-) (-) dest src --update dest (fetch dest - fetch src) s
        Inst [Seg seg] Ijmp [Mem (Memory Bits16 r RegNone 0 (Immediate Bits16 v))] ->
            s { ip = fetchMem seg (fromIntegral (fetch $ Reg r) + fromIntegral v) }
        Inst [] Ijmp [Jump (Immediate Bits8 v)] ->
            s { ip = ip + extend8_16 (fromIntegral v) }
        Inst [] Ijge [Jump (Immediate Bits8 v)] ->
            if signF == overflowF then s { ip = ip + extend8_16 (fromIntegral v) } else s
        Inst [] Ijl [Jump (Immediate Bits8 v)] ->
            if signF /= overflowF then s { ip = ip + extend8_16 (fromIntegral v) } else s
        Inst [] Ijz [Jump (Immediate Bits8 v)] ->
            if zeroF == 1 then s { ip = ip + extend8_16 (fromIntegral v) } else s
      where
        getFlag i = (flags `shiftR` i) .&. 1
        carryF    = getFlag 0
        zeroF     = getFlag 6
        signF     = getFlag 7
        overflowF = getFlag 11

        adc a b = a + b + fromIntegral carryF

        fetchMem seg i = fromIntegral low + (fromIntegral high `shiftL` 8)
          where
            (low: high: _) = BS.unpack $ BS.drop (fetchS seg + i) code

        fetchMem8 seg i = fromIntegral low
          where
            (low: _) = BS.unpack $ BS.drop (fetchS seg + i) code

        fetchS x = case x of
            CS  -> fromIntegral cs `shiftL` 4
            DS  -> 0x2a01b

        fetch :: Operand -> Word16
        fetch x = case x of
            Reg (Reg16 r) -> reg r
            Reg (Reg8 r L) -> reg r .&. 0xff
            Reg (Reg8 r H) -> reg r `shiftR` 8
            Mem (Memory Bits16 (Reg16 RBP) RegNone 0 (Immediate Bits8 v))
                -> stack V.! ((fromIntegral bp + fromIntegral v) `shiftR` 1)
            Mem (Memory Bits16 (Reg16 RBX) RegNone 0 (Immediate Bits16 v))
                -> fetchMem DS $ fromIntegral bx + fromIntegral v
            Mem (Memory Bits8 (Reg16 RBX) RegNone 0 (Immediate Bits16 v))
                -> fetchMem8 DS $ fromIntegral bx + fromIntegral v
            Imm (Immediate Bits8 v) -> fromIntegral v
            Imm (Immediate Bits16 v) -> fromIntegral v
            Const (Immediate Bits0 0) -> 1 -- !!!
          where
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
            = s { flags = (flags .&. 0xef3e)
                      .|. (if car then 1 else 0)      -- carry flag
                      .|. ((x .&. 0x8000) `shiftR` 3)      -- sign flag
                      .|. (if x == 0 then 2^6 else 0)      -- zero flag
                      .|. (if ovf then 2^11 else 0)        -- overflow flag
                }

        twoOp :: Bool -> (Word16 -> Word16 -> Word16) -> (Int -> Int -> Int) -> Operand -> Operand -> State
        twoOp store op op' x y = (if store then update x r else id) $ updateFlags carry overflow r s
          where
            a = fetch x
            b = fetch y
            r = op a b
            r' = fromIntegral $ op' (fromIntegral' a) (fromIntegral' b)
            r'' = fromIntegral $ op' (fromIntegral a) (fromIntegral b)
            overflow = r /= r'
            carry = r /= r''

        update :: Operand -> Word16 -> State -> State
        update x y s@State{..} = case x of
            Reg (Reg16 r) -> case r of
                RBP -> s { bp = y }
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

fromIntegral' :: Word16 -> Int
fromIntegral' v = if v >= 0x8000 then fromIntegral v - 0x10000 else fromIntegral v

writeVec :: Int -> a -> V.Vector a -> V.Vector a
writeVec i a = V.modify $ \v -> MV.write v i a

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
    , heap  :: !(V.Vector Word8)
    , stack :: !(V.Vector Word16)
    }

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
polarAngle z y = case (z < 0, y < 0, z > y) of
    (False, False, False) ->  result
    (False, False,  True) -> -result + 0x100
    (False,  True, False) -> -result + 0x200
    (False,  True,  True) ->  result + 0x100
    ( True, False, False) -> -result
    ( True, False,  True) ->  result - 0x100
    ( True,  True, False) ->  result - 0x200
    ( True,  True,  True) -> -(result + 0x100)
  where
    (z', y') = if z > y then (y, z) else (z, y)

    result
        | z == y = 0x80
        | otherwise = atantable V.! index

    index = round ((fromIntegral z' `shiftL` 16) `div` y') `shiftR` 8

    round x | (x .&. 0xFF) >= 0x80 = x + 0x100
            | otherwise            = x

atantable :: V.Vector Int
atantable = V.fromList [0, 1, 1, 2, 3, 3, 4, 4, 5, 6, 6, 7, 8, 8, 9, 10, 10, 11, 11, 12, 13, 13, 14, 15, 15, 16, 16, 17, 18, 18, 19, 20, 20, 21, 22, 22, 23, 23, 24, 25, 25, 26, 27, 27, 28, 28, 29, 30, 30, 31, 31, 32, 33, 33, 34, 34, 35, 36, 36, 37, 38, 38, 39, 39, 40, 41, 41, 42, 42, 43, 44, 44, 45, 45, 46, 46, 47, 48, 48, 49, 49, 50, 51, 51, 52, 52, 53, 53, 54, 55, 55, 56, 56, 57, 57, 58, 58, 59, 60, 60, 61, 61, 62, 62, 63, 63, 64, 65, 65, 66, 66, 67, 67, 68, 68, 69, 69, 70, 70, 71, 71, 72, 72, 73, 74, 74, 75, 75, 76, 76, 77, 77, 78, 78, 79, 79, 80, 80, 81, 81, 82, 82, 83, 83, 84, 84, 84, 85, 85, 86, 86, 87, 87, 88, 88, 89, 89, 90, 90, 91, 91, 91, 92, 92, 93, 93, 94, 94, 95, 95, 96, 96, 96, 97, 97, 98, 98, 99, 99, 99, 100, 100, 101, 101, 102, 102, 102, 103, 103, 104, 104, 104, 105, 105, 106, 106, 106, 107, 107, 108, 108, 108, 109, 109, 110, 110, 110, 111, 111, 112, 112, 112, 113, 113, 113, 114, 114, 115, 115, 115, 116, 116, 116, 117, 117, 118, 118, 118, 119, 119, 119, 120, 120, 120, 121, 121, 121, 122, 122, 122, 123, 123, 123, 124, 124, 124, 125, 125, 125, 126, 126, 126, 127, 127, 127, 128, 128]

