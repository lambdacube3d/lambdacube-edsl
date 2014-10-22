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
import Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))
import GHC.Int (Int64)

----------------------------------------------

gameExe = unsafePerformIO $ BS.readFile "original/game.exe"

(0x5a4d: bytesInLastPage: pagesInExecutable: relocationEntries:
 paragraphsInHeader: additionalMemoryNeeded: maxAdditionalMemoryNeeded: ssInit:
 spInit: checksum: ipInit: csInit:
 firstRelocationItemOffset: overlayNumber: _)
    = map combine $ everyNth 2 $ BS.unpack $ gameExe
  where
    combine [a,b] = fromIntegral b `shiftL` 8 .|. fromIntegral a :: Word16

headerSize = fromIntegral paragraphsInHeader `shiftL` 4      -- 0x2890
executableSize = (fromIntegral pagesInExecutable `shiftL` 9) + if (bytesInLastPage > 0) then fromIntegral bytesInLastPage - 0x200 else 0 :: Int -- 0x1f0 ?


dataSegment = 0x2b77

everyNth n [] = []
everyNth n xs = take n xs: everyNth n (drop n xs)

segments :: V.Vector Word16
segments = V.fromList [0, 0, 0, 0, 0,  0, 0x15f2{-?-}, 0, 0, 0,  0, 0x1ea0{-?-}, 0x1ea2, csInit]

symbols =
    [ (,) "sin"               (12, 0x3cbe)    -- 0x24f6e - hs
    , (,) "cos"               (12, 0x3d0c)
    , (,) "polarAngle"        (12, 0x2e)      -- 0x212de - hs
    , (,) "polarRadius2D"     (12, 0x1696)
    , (,) "polarRadius3D"     (11, 0x8)       -- 0x21298 - hs
    , (,) "rectComparePoint"  ( 6, 0xe) -- 0x187be - hs   -- x * 16 + y = 0x15ee2
    , (,) "main"              (13, ipInit)
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

data MachineState = MachineState
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

initState :: (Int, Word16) -> MachineState
initState (seg, ip) = MachineState
    { stack = V.replicate (2^10) 0
    , heap  = IM.empty
    , code  = BS.drop headerSize gameExe

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

call :: String -> MachineState
call x = initState $ head [addr | (x',addr) <- symbols, x == x']

stateDiff :: MachineState -> MachineState -> String
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
    f1 MachineState{..} = [ax,bx,cx,dx,si,di,ds,es,bp,cs,ss,sp]
    f2 MachineState{..} = [overflowF,signF,zeroF,carryF]
      where
        getFlag c i = [if (flags `shiftR` i) .&. 1 == 0 then toLower c else c]
        carryF    = getFlag 'C' 0
        zeroF     = getFlag 'Z' 6
        signF     = getFlag 'S' 7
        overflowF = getFlag 'O' 11
    f3 MachineState{..} = V.toList stack

take' n l = case splitAt n l of
    (xs, []) -> xs
    (xs, _) -> xs ++ ["..."]

showHex' i x = replicate (i - length s) '0' ++ s where s = showHex x ""

showReg "" _ = "       "
showReg n r = n ++ ":" ++ showHex' 4 r

pad i s = s ++ take (i - length s) (repeat ' ')

numOfDisasmLines = 30

type Machine = State MachineState

instance Show MachineState where
    show s@MachineState{..} = unlines
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

coode sc s@MachineState{..} = zipWith (\x (b, y) -> (if sc then pad 14 (map toUpper $ mdHex x) ++ " " else "")
    ++ (if b || sc then pad 25 (mdAssembly x) ++ " " else "") ++ y) (disassembleMetadata disasmConfig $ BS.drop ips code) (sts sc s)
 where
        ips = (fromIntegral cs `shiftL` 4) + fromIntegral ip :: Int

sts :: Bool -> MachineState -> [(Bool, String)]
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
    (@.) :: MachineState -> a -> MachineState
instance PushVal Word16 where
    s@MachineState{..} @. x = s { stack = writeVec (fromIntegral $ (sp + 2) `div` 2) x stack, sp = sp - 2 }
instance PushVal (V.Vector Word16) where
    s@MachineState{..} @. v = s { stack = writeVec (fromIntegral $ (sp + 2) `div` 2) x stack, sp = sp - 2, heap = h `IM.union` heap }
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

push x = modify $ \s@MachineState{..} -> s { sp = sp - 2, stack = writeVec (fromIntegral $ (sp - 2) `shiftR` 1) x stack }
pop = state $ \s@MachineState{..} -> (stack V.! fromIntegral (sp `shiftR` 1), s { sp = sp + 2 })

quotRem' :: Integral a => a -> a -> (a, a)
quotRem' a 0 = (0, 0)
quotRem' a b = quotRem a b

step_ :: MachineState -> (Metadata, MachineState)
step_ s@MachineState{..} = (md, flip execState s' $ exec (mdInst md) s')
  where
    Just (md, rem) = disassembleOne disasmConfig $ BS.drop ips code

    ips = (fromIntegral cs `shiftL` 4) + fromIntegral ip :: Int
    ip' = ip + fromIntegral (mdLength md)
    s' = s { ip = ip' }

    exec :: Instruction -> MachineState -> Machine ()
    exec i@Inst{..} s@MachineState{..} = case i of
        Inst [] Iint [Imm (Immediate {iSize = Bits8, iValue = v})] -> case v of
            0x21    -> case ah of
                0x30 -> modify $ \s -> s { ax = 0x0004, bx = 0xff00, cx = 0 }
                0x4a -> setFlag carryPos     False      -- unlimited memory available
                _       -> error $ "dos function #" ++ showHex ah "" --return ()
            _       -> error $ "interrupt #" ++ showHex v "" --return ()

        Inst ms Ipush [x] -> push (fetch x)
        Inst [] Ipop [x] -> pop >>= update x
        Inst ms Imov [dest, src] -> update dest (fetch src)
        Inst [] Iles [dest, Mem src] -> do
            let ad = compAddr src
            update dest $ fetchMem' ad
            updateSegment ES $ fetchMem' $ addAddr 2 ad
        Inst ms Ixchg [dest, src] -> do
            update src  $ fetch dest
            update dest $ fetch src
        Inst ms Idiv [src] -> do
            let (d, m) = (((fromIntegral dx :: Word32) `shiftL` 16) .|. fromIntegral ax) `quotRem'` fromIntegral (fetch src)
            update (Reg $ Reg16 RAX) (fromIntegral d)
            update (Reg $ Reg16 RDX) (fromIntegral m)
        Inst ms Imul [src] -> do
            let r = fromIntegral ax * fromIntegral (fetch src) :: Int
                high = fromIntegral $ r `shiftR` 16 
                low = fromIntegral $ r
            update (Reg $ Reg16 RAX) low
            update (Reg $ Reg16 RDX) high
        Inst ms Ineg [dest]      -> update dest (- fetch dest)
        Inst ms Idec [dest]      -> update dest (fetch dest - 1)
        Inst [] Icwd []          -> update (Reg $ Reg16 RDX) (if ax >= 0x8000 then 0xffff else 0)
        Inst [] Icbw []          -> update (Reg $ Reg16 RAX) (extend8_16 $ fromIntegral ax)

        Inst _ Ijmp [dest] ->
            setIp $ fetch dest
        Inst [] Icall [Ptr (Pointer seg (Immediate Bits16 v))] -> do
            push cs
            push ip
            setCs $ fromIntegral seg
            setIp $ fromIntegral v
        Inst [] Iretf arg -> do
            pop >>= setIp
            pop >>= setCs
            case arg of
                [] -> return ()
                [Imm i] -> setSp $ sp + imm i

        _ -> case inOpcode of
            Ija   -> condJump $ not carryF && not zeroF
            Ijae  -> condJump $ not carryF
            Ijb   -> condJump $ carryF
            Ijbe  -> condJump $ carryF || zeroF
            Ijcxz -> condJump $ cx == 0
            Ijg   -> condJump $ signF == overflowF && not zeroF
            Ijge  -> condJump $ signF == overflowF
            Ijl   -> condJump $ signF /= overflowF
            Ijle  -> condJump $ signF /= overflowF || zeroF
            Ijno  -> condJump $ not overflowF
            Ijnp  -> condJump $ not parityF
            Ijns  -> condJump $ not signF
            Ijnz  -> condJump $ not zeroF
            Ijo   -> condJump $ overflowF
            Ijp   -> condJump $ parityF
            Ijs   -> condJump $ signF
            Ijz   -> condJump $ zeroF

            Iadd  -> twoOp True (+)
            Iadc  -> twoOp True $ \a b -> a + b + fromIntegral (fromEnum carryF)
            Iand  -> twoOp True (.&.)
            Icmp  -> twoOp False (-)
            Ior   -> twoOp True (.|.)
            Isbb  -> twoOp True $ \a b -> a - b - fromIntegral (fromEnum carryF)
            Isub  -> twoOp True (-)
            Ixor  -> twoOp True xor

            Isal  -> shiftOp $ \x -> (testHighBit x, x `shiftL` 1)
            Isar  -> shiftOp $ \x -> (testBit x 0, setHighBit (testHighBit x) $ x `shiftR` 1)
            Ishl  -> shiftOp $ \x -> (testHighBit x, x `shiftL` 1)
            Ishr  -> shiftOp $ \x -> (testBit x 0, x `shiftR` 1)
            Ircl  -> shiftOp $ \x -> (testHighBit x, setBit' carryF (x `shiftL` 1) 0)
            Ircr  -> shiftOp $ \x -> (testBit x 0, setHighBit carryF $ x `shiftR` 1)
            Irol  -> shiftOp $ \x -> (testHighBit x, setBit' (testHighBit x) (x `shiftL` 1) 0)
            Iror  -> shiftOp $ \x -> (testBit x 0, setHighBit (testBit x 0) $ x `shiftR` 1)

            Iclc  -> setFlag carryPos     False
            Icld  -> setFlag directionPos False
            Icli  -> setFlag interruptPos False
            Icmc  -> setFlag carryPos     (not carryF)
            Istc  -> setFlag carryPos     True
            Istd  -> setFlag directionPos True
            Isti  -> setFlag interruptPos True

            Inop  -> return ()

            _ -> error $ "step: " ++ show i
      where
        setIp x = modify $ \s -> s { ip = x }
        setCs x = modify $ \s -> s { cs = x }
        setSp x = modify $ \s -> s { sp = x }

        ah = ax `shiftR` 8

        condJump b = case (inPrefixes, inOperands) of
            ([], [Jump (Immediate Bits8 v)]) -> if b then modify $ \s -> s { ip = ip + extend8_16 (fromIntegral v) } else return ()

        shiftOp :: (forall a . (Integral a, FiniteBits a) => a -> (Bool, a)) -> State MachineState ()
        shiftOp op = case inOperands of
            [x,y] -> do
                updateFlags' carry
                update x r
              where
                a = fetch x
                b = fromIntegral $ fetch y :: Int
                (carry, r) = case size x of
                    16 -> times b op a
                    8 -> id *** fromIntegral $ times b op (fromIntegral a :: Word8)

        times 1 f x = f x
        times n f x = times (n-1) f $ snd $ f x

        twoOp :: Bool -> (forall a . (Integral a, FiniteBits a) => a -> a -> a) -> State MachineState ()
        twoOp store op = case inOperands of
            [x,y] -> do
                updateFlags carry overflow r
                when store $ update x r
              where
                a = fetch x
                b = fetch y
                r = case size x of
                    16 -> op a b
                    8 -> fromIntegral $ op (fromIntegral a :: Word8) (fromIntegral b)
                r' = fromIntegral $ op (fromIntegral' a) (fromIntegral' b)
                r'' = fromIntegral $ op (fromIntegral a :: Int) (fromIntegral b)
                overflow = r /= r'
                carry = r /= r''

        getFlag i = testBit flags i
        setFlag i b = modify $ \s -> s { flags = setBit' b flags i }

        carryF    = getFlag carryPos
        zeroF     = getFlag zeroPos
        signF     = getFlag signPos
        overflowF = getFlag overflowPos
        parityF   = getFlag parityPos

        fetchMem' (a,b,c) = fetchMem a b c

        writeMem (Bits16, SS, i) v s@MachineState{..} = s { stack = writeVec (fromIntegral $ i `shiftR` 1) v stack }
        writeMem (Bits8, DS, i) v s@MachineState{..} = s { heap = IM.insert i (fromIntegral v) heap }
        writeMem (Bits16, DS, i) v s@MachineState{..} = s { heap = IM.insert i (fromIntegral v) $ IM.insert (i+1) (fromIntegral $ v `shiftR` 8) heap }
        writeMem x _ _ = error $ "writeMem: " ++ show x

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
        compAddr (Memory Bits16 RegNone RegNone 0 i) = (Bits16, DS, imm' i)
        compAddr m = error $ "compAddr: " ++ show m

        fetch :: Operand -> Word16
        fetch x = case x of
            Reg (RegSeg ES) -> es
            Reg (RegSeg DS) -> ds
            Reg (RegSeg SS) -> ss
            Reg (RegSeg CS) -> cs
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
            Mem (Memory size r RegNone 0 (Immediate Bits16 v))
                -> fetchMem size (ff DS) $ fromIntegral (fetch $ Reg r) + fromIntegral v
            Imm i -> imm i
            Const (Immediate Bits0 0) -> 1 -- !!!
            Jump (Immediate Bits8 v) -> ip + extend8_16 (fromIntegral v)
            _ -> error $ "fetch: " ++ show x
          where
            ff x = case inPrefixes of
                [] -> x
                [Seg s] -> s
            reg r = case r of
                RBP -> bp
                RSP -> sp
                RAX -> ax
                RBX -> bx
                RCX -> cx
                RDX -> dx
                RSI -> si
                RDI -> di

        updateFlags :: Bool -> Bool -> Word16 -> State MachineState ()
        updateFlags car ovf x = modify $ \s
            -> s { flags = (flags .&. 0xf73e)
                      .|. (if car then 1 else 0)      -- carry flag
                      .|. ((x .&. 0x8000) `shiftR` 4)      -- sign flag
                      .|. (if x == 0 then 2^6 else 0)      -- zero flag
                      .|. (if ovf then 2^11 else 0)        -- overflow flag
                }

        updateFlags' car = modify $ \s ->
              s { flags = (flags .&. 0xfffe)
                      .|. (if car then 1 else 0)      -- carry flag
                }

        size (Reg (Reg16 _)) = 16
        size (Reg (Reg8 _ _)) = 8
        size (Mem (Memory Bits16 _ _ _ _)) = 16
        size s = error $ "size: " ++ show s

        updateSegment ES x = modify $ \s@MachineState{..} -> s { es = x }

        update :: Operand -> Word16 -> State MachineState ()
        update x y = modify f where
          f s@MachineState{..} = case x of
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
                RCX -> s { cx = (cx .&. 0xff00) .|. y }
                RDX -> s { dx = (dx .&. 0xff00) .|. y }
            Reg (Reg8 r H) -> case r of
                RAX -> s { ax = (ax .&. 0xff) .|. (y `shiftL` 8) }
                RBX -> s { bx = (bx .&. 0xff) .|. (y `shiftL` 8) }
            Reg (RegSeg r) -> case r of
                SS -> s { ss = y }
                DS -> s { ds = y }
                ES -> s { es = y }
                CS -> s { cs = y }
            Mem m -> writeMem (compAddr m) y s
            x -> error $ "update: " ++ show x

[carryPos,_,parityPos,_,_,_,zeroPos,signPos,_,interruptPos,directionPos,overflowPos,_,_,_,_] = [0..15] :: [Int]

setBit' :: Bits a => Bool -> a -> Int -> a
setBit' True  = setBit
setBit' False = clearBit

testHighBit :: FiniteBits a => a -> Bool
testHighBit x = testBit x (finiteBitSize x - 1)

setHighBit :: FiniteBits a => Bool -> a -> a
setHighBit b x = setBit' b x (finiteBitSize x - 1)

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

