{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
import Numeric
import Data.Word
import Data.Int
import Data.Bits hiding (bit)
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Applicative hiding (Const)
import Control.Arrow
import Control.Monad.State
import Control.Monad.Error
import Control.Lens as Lens
import Test.QuickCheck hiding ((.&.))
import System.IO.Unsafe

import Hdis86
import Hdis86.Incremental

----------------------------------------------

gameExe = unsafePerformIO $ BS.readFile "original/game.exe"

(0x5a4d: bytesInLastPage: pagesInExecutable: relocationEntries:
 paragraphsInHeader: additionalMemoryNeeded: maxAdditionalMemoryNeeded: ssInit:
 spInit: checksum: ipInit: csInit:
 firstRelocationItemOffset: overlayNumber: _)
    = map combine $ everyNth 2 $ BS.unpack $ gameExe
  where
    combine [low, high] = (high, low) ^. combine2x8

headerSize = paragraph paragraphsInHeader    -- 0x2890
executableSize = (fromIntegral pagesInExecutable `shiftL` 9) + if (bytesInLastPage > 0) then fromIntegral bytesInLastPage - 0x200 else 0 :: Int -- 0x1f0 ?

dataSegment = 0x2b77

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

showHex' i x = replicate (i - length s) '0' ++ s where s = showHex x ""

pad i s = s ++ take (i - length s) (repeat ' ')

----------------------------------------------

uComb :: Lens' a b -> Lens' a c -> Lens' a (b, c)
uComb x y = lens ((^. x) &&& (^. y)) $ \a (b, c) -> set x b . set y c $ a

(>-) :: Getting a s a -> (a -> t1 -> s -> t) -> t1 -> s -> t
(k >- f) tr a = f (a ^. k) tr a

ret :: a -> Getting a s a
ret a _ _ = Lens.Const a

---------------------------------

class AsSigned a where
    asSigned :: a -> Int

instance AsSigned Word8 where
    asSigned x = fromIntegral (fromIntegral x :: Int8)
instance AsSigned Word16 where
    asSigned x = fromIntegral (fromIntegral x :: Int16)
instance AsSigned Word32 where
    asSigned x = fromIntegral (fromIntegral x :: Int32)

setBit' :: Bits a => Bool -> a -> Int -> a
setBit' True  = setBit
setBit' False = clearBit

highBit :: FiniteBits a => a -> Bool
highBit x = testBit x (finiteBitSize x - 1)

setHighBit :: FiniteBits a => Bool -> a -> a
setHighBit b x = setBit' b x (finiteBitSize x - 1)

combine2x8 :: Iso' (Word8, Word8) Word16
combine2x8 = iso (\(hi,lo) -> fromIntegral hi `shiftL` 8 .|. fromIntegral lo) (\d -> (fromIntegral $ d `shiftR` 8, fromIntegral d))

combine2x16 :: Iso' (Word16, Word16) Word32
combine2x16 = iso (\(hi,lo) -> fromIntegral hi `shiftL` 16 .|. fromIntegral lo) (\d -> (fromIntegral $ d `shiftR` 16, fromIntegral d))

high, low :: Lens' Word16 Word8
high = from combine2x8 . _1
low = from combine2x8 . _2

bit :: Bits a => Int -> Lens' a Bool
bit i = lens (`testBit` i) $ \x b -> if b then x `setBit` i else x `clearBit` i

paragraph :: Word16 -> Int
paragraph i = fromIntegral i `shiftL` 4

segAddr_ :: Word16 -> Int -> Int
segAddr_ s w = paragraph s + w

segAddr :: Word16 -> Word16 -> Int
segAddr s w = segAddr_ s $ fromIntegral w

----------------

type Mem = S.Seq Word8

byteAt :: Int -> Lens' Mem Word8
byteAt i = lens (`S.index` i) $ \x b -> S.update i b x          -- could be faster

wordAt :: Int -> Lens' Mem Word16
wordAt i = (byteAt (i+1) `uComb` byteAt i) . combine2x8       -- could be faster

dwordAt :: Int -> Lens' Mem Word32
dwordAt i = (wordAt (i+2) `uComb` wordAt i) . combine2x16       -- could be faster

----------------

data Size
    = S1 | S8 | S16 | S32
        deriving (Eq, Ord)

data Key
    = KReg Size Int
    | Flag Int
    | Heap Size Int
        deriving (Eq, Ord)

instance Show Key where
    show (Heap _ i) = "[" ++ showHex' 5 i ++ "]"
    show (Flag i) = ["????odi?sz???p?c" !! (15-i)]
    show (KReg S8 i) = ["al","ah","dl","dh","bl","bh","cl","ch"] !! i
    show (KReg S16 i) = ["ax","dx","bx","cx", "si","di", "cs","ss","ds","es", "ip","sp","bp"] !! (i `div` 2)
    show (KReg S32 0) = "ax:dx"

keySize (Heap s _) = s
keySize (Flag _)  = S1
keySize (KReg s _) = s

data Config_ = Config_
    { _numOfDisasmLines :: Int
    }

$(makeLenses ''Config_)

defConfig = Config_
    { _numOfDisasmLines = 30
    }

data MachineState = MachineState
    { _flags :: Word16
    , _regs  :: Mem
    , _heap  :: Mem
    , _hist  :: Set.Set Key
    , _config :: Config_
    }

$(makeLenses ''MachineState)

steps = config . numOfDisasmLines

clearHist = hist .= Set.empty

initState :: MachineState
initState = flip execState (MachineState undefined undefined undefined undefined defConfig) $ do
    clearHist
    heap .= S.fromList (BS.unpack (BS.drop headerSize gameExe) ++ replicate (paragraph additionalMemoryNeeded) 63)
    regs .= S.fromList (replicate 26 0)
    ss .= ssInit
    sp .= spInit
    cs .= csInit
    ip .= ipInit
    ds .= dataSegment
    flags .= 0x7202
    clearHist

type MachinePart a = Lens' MachineState a

addHist :: Key -> Lens' MachineState a -> Lens' MachineState a
addHist sh k = lens (^. k) $ \s a -> hist %~ Set.insert sh $ set k a s

[_,_,_,_,overflowF,directionF,interruptF,_,signF,zeroF,_,_,_,parityF,_,carryF]
    = [ addHist (Flag i) $ flags . bit i | i <- [15,14..0] ]
[ax,dx,bx,cx, si,di, cs,ss,ds,es, ip,sp,bp]
    = [ addHist (KReg S16 i) $ regs . wordAt i | i <- [0,2..24] ]
[al,ah,dl,dh,bl,bh,cl,ch]
    = [ addHist (KReg S8 i) $ regs . byteAt i | i <- [0..7] ]
dxax = addHist (KReg S32 0) $ regs . dwordAt 0
xx :: MachinePart Word16
xx = lens (const $ error "xx") $ \s _ -> s

flagsl = flags . low

heap8  i
    = addHist (Heap S8 i) $ heap . byteAt i
heap16 i
    = addHist (Heap S16 i) $ heap . wordAt i

stackTop :: MachinePart Word16
stackTop = ss >- \ss -> sp >- \sp -> heap16 $ segAddr ss sp

----------------------

data Halt
    = Halt
    | Err String
  deriving Show

instance Error Halt where

type Machine = ErrorT Halt (State MachineState)

push :: Word16 -> Machine ()
push x = do
    sp %= (+ (-2))
    stackTop .= x

pop :: Machine Word16
pop = do
    x <- use stackTop
    sp %= (+ 2)
    return x

exec = flip execState initState . runErrorT

call :: String -> Machine ()
call name = do
    let Just (seg, ipInit) = lookup name symbols
    push 0xffff
    push 0xfffe
    cs .= segments V.! seg
    ip .= ipInit

instance Show MachineState where
    show s = intercalate "\n" $
        [ "  Flags: " ++ zipWith showFlag "odiszpc" [overflowF,directionF,interruptF,signF,zeroF,parityF,carryF]
        , ("  "++) $ unwords $ zipWith showReg ["AX","BX","CX","DX"] [ax,bx,cx,dx]
        , ("  "++) $ unwords $ zipWith showReg ["SI","DI","IP","SP","BP"] [si,di,ip,sp,bp]
        , ("  "++) $ unwords $ zipWith showReg ["DS","ES","CS","SS"] [ds,es,cs,ss]

        , (\(a,b)->a ++ "\n" ++ b) $ (("Stack: " ++) . unwords) *** (("       " ++) . unwords) $ unzip
                            $ take 20
                            $   zip (map (showHex' 4) [s ^. sp, s ^. sp + 2..0xffff] ++ repeat "####")
                                    (map ((showHex' 4) . (s ^.) . heap16) [sps, sps + 2.. S.length (s ^. heap) - 1] ++ repeat "####")
        , "Code: "
        ] ++ map (take 149 . ("  " ++)) (take' (s ^. config . numOfDisasmLines) $ coode s)
      where
        sps = segAddr (s ^. ss) (s ^. sp)
        showFlag c k = if s ^. k then toUpper c else c
        showReg c k = c ++ ":" ++ showHex' 4 (s ^. k)

coode :: MachineState -> [String]
coode s = case x of
    Left e -> show e: []
    Right x -> (pad 14 (map toUpper $ mdHex x) ++ " "
        ++ pad 27 (mdAssembly x) ++ " " ++ unwords (map shKey $ Set.toList $ s' ^. hist)): case y of
        Left e -> show e: []
        Right () -> coode s'
 where
    (x, s_) = flip runState s $ runErrorT $ fetchInstr

    (y, s') = flip runState s_ $ runErrorT $ do
        clearHist
        exec_ $ either (error . show) mdInst $ x

    shKey k = case k of
        Heap S8 i -> diff (sh 2) $ heap . byteAt i
        Heap S16 i -> diff (sh 4) $ heap . wordAt i
        Flag i -> diff sf $ flags . bit i
        KReg S8 i -> diff (sh 2) $ regs . byteAt i
        KReg S16 i -> diff (sh 4) $ regs . wordAt i
        KReg S32 i -> diff (sh 8) $ regs . dwordAt i
      where
        diff :: Eq a => (a -> String) -> Lens' MachineState a -> String
        diff f l = par (v == v') $ f v'
          where
            v = s ^. l
            v' = s' ^. l

        sh :: (Show a, Integral a) => Int -> a -> String
        sh i v = show k ++ ":" ++ showHex' i v
        sf True = map toUpper $ show k
        sf False = show k

        par True a = "(" ++ a ++ ")"
        par False a = a

{-
sts :: Bool -> MachineState -> [(Bool, String)]
sts sc s
    | cs s == cs s' && ip s' == ip s + (fromIntegral $ mdLength i) = (True, stateDiff s s'): sts sc s'
    | inOpcode (mdInst i) == Icall = (True, stateDiff s s''): sts sc s''
    | otherwise = (True, "JUMP"): map ((,) False . ("|" ++)) (coode False s')
  where
    (i, s') = step_ s
    s'' = stepthrough s

stepthrough = step 1 . findRet 0 . step 1
-}

findRet n = do
    op <- step_
    case inOpcode $ mdInst op of
        Iretf | n == 0 -> return ()
        Iretf -> findRet (n-1)
        Icall -> findRet (n+1)
        _ -> findRet n

class PushVal a where
    push_ :: a -> Machine ()
instance PushVal Word16 where
    push_ = push

instance PushVal (V.Vector Word16) where
    push_ v = do
        x_ <- S.length <$> use heap
        s <- use ds
        push $ fromIntegral $ x_ - paragraph s
        heap %= (S.>< h)
      where
        h = S.fromList $ concatMap spl $ V.toList v

        spl :: Word16 -> [Word8]
        spl w = [fromIntegral w, fromIntegral (w `shiftR` 8)]


eval :: Machine () -> Word16
eval s = either (const (-1)) id $ flip evalState initState $ runErrorT $ do
    s
    findRet 0
    use ax

{-
step 0 s = s
step n s = step (n-1) $ snd $ step_ s
-}

quotRem' :: Integral a => a -> a -> (a, a)
quotRem' a 0 = (0, 0)
quotRem' a b = quotRem a b

fetchInstr :: Machine Metadata
fetchInstr = do
    c <- use cs
    p <- use ip
    when ((c,p) == (0xffff,0xfffe)) $ throwError Halt 
    let ips = segAddr c p
    Just (md, _) <- disassembleOne disasmConfig . BS.pack . take 20 . seqToList . S.drop ips <$> use heap
    ip %= (+ fromIntegral (mdLength md))
    return md

step_ :: Machine Metadata
step_ = do
    md <- fetchInstr
    exec_ $ mdInst md
    return md

exec_ :: Instruction -> Machine ()
exec_ i@Inst{..}
    | Rep `elem` inPrefixes = cycle
    | RepE `elem` inPrefixes = error "exec_"
    | RepNE `elem` inPrefixes = error "exec_"
    | otherwise = body
  where
    body = execRep $ i { inPrefixes = filter (not . rep) inPrefixes }

    cycle = do
        n <- use cx
        when (n /= 0) $ do
            body
            cx %= (+(-1))
            cycle

    rep p = p `elem` [Rep, RepE, RepNE]

execRep :: Instruction -> Machine ()
execRep i@Inst{..} = case i of
    Inst [] Iint [Imm (Immediate {iSize = Bits8, iValue = v})] -> case v of
        0x21 -> do
          v <- use ah
          case v of
            0x30 -> do
                al .= 0x04      -- major version number (2-5)
                ah .= 0x00      -- minor version number (in hundredths decimal)
                bh .= 0xff      -- 0xFF indicates MS-DOS
                do              -- 24 bit OEM serial number
                    bl .= 0
                    cx .= 0
            0x4a -> carryF .= False      -- unlimited memory available
            _    -> throwError $ Err $ "dos function #" ++ showHex v ""
        _ -> throwError $ Err $ "interrupt #" ++ showHex v ""

    Inst [] Ilea [dest, Mem src] -> do
        compAddr src >>= update dest . fromIntegral
    Inst [] Iles [dest, Mem src] -> do
        ad <- compAddr src
        use (heap16 ad) >>= update dest
        use (heap16 $ ad + 2) >>= update (Reg $ RegSeg ES) 
    Inst [] Ilds [dest, Mem src] -> do
        ad <- compAddr src
        use (heap16 ad) >>= update dest
        use (heap16 $ ad + 2) >>= update (Reg $ RegSeg DS) 
    Inst _ Ijmp [dest] ->
        fetch16 dest >>= (ip .=)
    Inst [] Icall [Ptr (Pointer seg (Immediate Bits16 v))] -> do
        use cs >>= push
        use ip >>= push
        cs .= fromIntegral seg
        ip .= fromIntegral v
    Inst [] Iretf arg -> do
        pop >>= (ip .=)
        pop >>= (cs .=)
        case arg of
            [] -> return ()
            [Imm i] -> sp %= (+ imm i)

    _ -> case inOpcode of

        Ipush -> fetch16 op1 >>= push
        Ipop  -> pop >>= update op1
        Ipusha -> sequence_ [use r >>= push | r <- [ax,cx,dx,bx,sp,bp,si,di]]
        Ipopa  -> sequence_ [pop >>= (r .=) | r <- [di,si,bp,xx,bx,dx,cx,ax]]
        Ipushfw -> use flags >>= push
        Ipopfw  -> pop >>= (flags .=)

        Ija   -> condJump $ (&&) <$> (not <$> use carryF) <*> (not <$> use zeroF)
        Ijae  -> condJump $ not <$> use carryF
        Ijb   -> condJump $ use carryF
        Ijbe  -> condJump $ (||) <$> use carryF <*> use zeroF
        Ijcxz -> condJump $ (== 0) <$> use cx
        Ijg   -> condJump $ (&&) <$> ((==) <$> use signF <*> use overflowF) <*> (not <$> use zeroF)
        Ijge  -> condJump $ (==) <$> use signF <*> use overflowF
        Ijl   -> condJump $ (/=) <$> use signF <*> use overflowF
        Ijle  -> condJump $ (||) <$> ((/=) <$> use signF <*> use overflowF) <*> use zeroF
        Ijno  -> condJump $ not <$> use overflowF
        Ijnp  -> condJump $ not <$> use parityF
        Ijns  -> condJump $ not <$> use signF
        Ijnz  -> condJump $ not <$> use zeroF
        Ijo   -> condJump $ use overflowF
        Ijp   -> condJump $ use parityF
        Ijs   -> condJump $ use signF
        Ijz   -> condJump $ use zeroF
        Iloop   -> condJump $ cx %= (+(-1)) >> (/= 0) <$> use cx
        Iloope  -> condJump $ cx %= (+(-1)) >> (&&) <$> use zeroF <*> ((/= 0) <$> use cx)
        Iloopnz -> condJump $ cx %= (+(-1)) >> (&&) <$> (not <$> use zeroF) <*> ((/= 0) <$> use cx)

        Iclc  -> carryF     .= False
        Icld  -> directionF .= False
        Icli  -> interruptF .= False
        Icmc  -> carryF     %= not
        Istc  -> carryF     .= True
        Istd  -> directionF .= True
        Isti  -> interruptF .= True

        Inop  -> return ()
        Ihlt  -> throwError Halt

        Isahf -> use ah >>= (flagsl .=)
        Ixlatb -> do
            x <- use al
            fetch8 (Mem $ Memory undefined (Reg16 RBX) RegNone 0 $ Immediate Bits16 $ fromIntegral x) >>= (al .=)

        Iint -> do
            v <- fetch8 op1
            interrupt v
        Iinto -> do
            b <- use overflowF
            when b $ interrupt 4
        Iiretw -> do
            pop >>= (ip .=)
            pop >>= (cs .=)
            pop >>= (flags .=)


        _ -> case size_ of
          Pack get set alx ahd axd -> case inOpcode of
            Imov -> get op2 >>= set op1
            Ixchg -> do
                a <- get op1
                b <- get op2
                set op1 b
                set op2 a
            Ineg -> get op1 >>= set op1 . negate
            Idec -> get op1 >>= set op1 . (+ (-1))
            Iinc -> get op1 >>= set op1 . (+1)

            Isal  -> shiftOp $ \_ x -> (highBit x, x `shiftL` 1)
            Isar  -> shiftOp $ \_ x -> (testBit x 0, setHighBit (highBit x) $ x `shiftR` 1)
            Ishl  -> shiftOp $ \_ x -> (highBit x, x `shiftL` 1)
            Ishr  -> shiftOp $ \_ x -> (testBit x 0, x `shiftR` 1)
            Ircl  -> shiftOp $ \c x -> (highBit x, setBit' c (x `shiftL` 1) 0)
            Ircr  -> shiftOp $ \c x -> (testBit x 0, setHighBit c $ x `shiftR` 1)
            Irol  -> shiftOp $ \_ x -> (highBit x, setBit' (highBit x) (x `shiftL` 1) 0)
            Iror  -> shiftOp $ \_ x -> (testBit x 0, setHighBit (testBit x 0) $ x `shiftR` 1)

            Iadd  -> twoOp True (+)
            Iadc  -> do
                c <- fromEnum <$> use carryF
                twoOp True $ \a b -> a + b + fromIntegral c
            Iand  -> twoOp True (.&.)
            Itest -> twoOp False (.&.)
            Icmp  -> twoOp False (-)
            Ior   -> twoOp True (.|.)
            Isbb  -> do
                c <- fromEnum <$> use carryF
                twoOp True $ \a b -> a - b - fromIntegral c
            Isub  -> twoOp True (-)
            Ixor  -> twoOp True xor

            Idiv  -> do
                x <- use axd
                y <- get op1
                let (d, m) = x `quotRem'` fromIntegral y
                alx .= fromIntegral d
                ahd .= fromIntegral m
            Iidiv  -> do
                x <- use axd
                y <- get op1
                let (d, m) = asSigned x `quotRem'` asSigned y
                alx .= fromIntegral d
                ahd .= fromIntegral m
            Imul  -> do
                x <- use alx
                y <- get op1
                axd .= fromIntegral (asSigned x * asSigned y)
            Iimul  -> do
                x <- use alx
                y <- get op1
                axd .= fromIntegral x * fromIntegral y

            oc | oc == Istosb || oc == Istosw -> do
                use alx >>= set (memIndex RDI)
                adjustIndex di
               | oc == Ilodsb || oc == Ilodsw -> do
                get (memIndex RSI) >>= (alx .=)
                adjustIndex si
               | oc == Icmpsb || oc == Icmpsw -> do
                twoOp_ False (-) (memIndex RDI) (memIndex RSI)
                adjustIndex si
                adjustIndex di
               | oc == Imovsb || oc == Imovsw -> do
                get (memIndex RSI) >>= set (memIndex RDI)
                adjustIndex si
                adjustIndex di
               | oc == Icwd || oc == Icbw -> do
                x <- use alx
                axd .= fromIntegral (asSigned x)

            Iin -> do
                v <- fetch16 op2
                input v >>= set op1
            Iout -> do
                c <- fetch16 op1
                v <- get op2
                output' c $ fromIntegral v

            oc | oc `elem` [Iaaa, Iaad, Iaam, Iaas, Idaa, Idas] -> error $ "step: not implemented: " ++ show i

            _ -> error $ "fatal error step: " ++ show i

           where

            shiftOp :: (forall a . (Integral a, FiniteBits a) => Bool -> a -> (Bool, a)) -> Machine ()
            shiftOp op = do
                a <- get op1
                n <- fromIntegral <$> fetch8 op2
                c <- use carryF
                let (c', r) = iterate (uncurry op) (c, a) !! n
                set op1 r
                carryF .= c'

            twoOp :: Bool -> (forall a . (Integral a, FiniteBits a) => a -> a -> a) -> Machine ()
            twoOp store op = twoOp_ store op op1 op2

            twoOp_ :: Bool -> (forall a . (Integral a, FiniteBits a) => a -> a -> a) -> Operand -> Operand -> Machine ()
            twoOp_ store op op1 op2 = do
                a <- get op1
                b <- get op2
                let r = op a b
                carryF    .= (fromIntegral r /= op (fromIntegral a :: Int) (fromIntegral b))
                overflowF .= (asSigned r /= op (asSigned a) (asSigned b))
                zeroF     .= (r == 0)
                signF     .= highBit r
                when store $ set op1 r

  where
    input v = do
        case v of
            _ -> throwError $ Err $ "input #" ++ showHex' 4 v

    output' v x = do
        case v of
            _ -> throwError $ Err $ "output #" ++ showHex' 4 v ++ " 0x" ++ showHex' 4 x

    interrupt v = do
        use flags >>= push
        use cs >>= push
        use ip >>= push
        interruptF .= False
        case v of
            _ -> throwError $ Err $ "interrupt #" ++ show v

    memIndex r = (Mem $ Memory undefined (Reg16 r) RegNone 0 $ Immediate Bits0 0)

    adjustIndex i = do
        d <- use directionF
        i %= if d then (+(-sizeByte)) else (+sizeByte)

    condJump mb = case (inPrefixes, inOperands) of
        ([], [Jump (Immediate Bits8 v)]) -> do
            b <- mb
            when b $ do
                ip %= (+ fromIntegral (fromIntegral v :: Int8))

    imm :: Immediate Word64 -> Word16
    imm (Immediate Bits8 v) = fromIntegral v
    imm (Immediate Bits16 v) = fromIntegral v

    imm8 :: Immediate Word64 -> Word8
    imm8 (Immediate Bits8 v) = fromIntegral v
    imm8 (Immediate Bits16 v) = fromIntegral v

    imm' :: Immediate Int64 -> Int
    imm' (Immediate Bits0 _) = 0
    imm' (Immediate Bits8 v) = fromIntegral v
    imm' (Immediate Bits16 v) = fromIntegral v

    op1 = inOperands !! 0
    op2 = inOperands !! 1

    compAddr (Memory _ RegNone RegNone 0 i)     = inSegment DS $ imm' i
    compAddr (Memory _ (Reg16 RBP) RegNone 0 i) = do
        x <- use bp
        inSegment SS $ imm' i + fromIntegral x
    compAddr (Memory _ (Reg16 RBX) RegNone 0 i) = do
        x <- use bx
        inSegment DS $ imm' i + fromIntegral x
    compAddr (Memory _ (Reg16 RSI) RegNone 0 i) = do
        x <- use si
        inSegment DS $ imm' i + fromIntegral x
    compAddr (Memory _ (Reg16 RDI) RegNone 0 i) = do
        x <- use di
        inSegment ES $ imm' i + fromIntegral x
    compAddr m = error $ "compAddr: " ++ show m

    fetch8 :: Operand -> Machine Word8
    fetch8 x = case x of
        Reg r -> use $ reg8 r
        Mem m -> compAddr m >>= use . heap8
        Imm i -> return $ imm8 i
        Hdis86.Const (Immediate Bits0 0) -> return 1 -- !!!
        _ -> error $ "fetch8: " ++ show x

    fetch16 :: Operand -> Machine Word16
    fetch16 x = case x of
        Reg r -> use $ reg r
        Mem m -> compAddr m >>= use . heap16
        Imm i -> return $ imm i
--        Hdis86.Const (Immediate Bits0 0) -> return 1 -- !!!
        Jump (Immediate Bits8 v) -> do
            x <- use ip
            return $ x + fromIntegral (fromIntegral v :: Int8)
        _ -> error $ "fetch: " ++ show x

    inSegment :: Segment -> Int -> Machine Int
    inSegment x v = do
        s <- fetch16 $ Reg $ RegSeg $ fromMaybe x $ listToMaybe [s | Seg s <- inPrefixes]
        return $ segAddr_ s v

    reg (Reg16 r) = case r of
        RBP -> bp
        RSP -> sp
        RAX -> ax
        RBX -> bx
        RCX -> cx
        RDX -> dx
        RSI -> si
        RDI -> di
    reg (RegSeg r) = case r of
        ES -> es
        DS -> ds
        SS -> ss
        CS -> cs
    reg x = error $ "reg: " ++ show x

    reg8 (Reg8 r L) = case r of
        RAX -> al
        RBX -> bl
        RCX -> cl
        RDX -> dl
    reg8 (Reg8 r H) = case r of
        RAX -> ah
        RBX -> bh
        RCX -> ch
        RDX -> dh

    size :: WordSize
    size = fromMaybe (error $ "size: " ++ show i) $ listToMaybe $ catMaybes $ map f inOperands ++ [g inOpcode] where
        f (Reg (Reg16 _)) = Just Bits16
        f (Reg (Reg8 _ _)) = Just Bits8
        f (Mem (Memory Bits16 _ _ _ _)) = Just Bits16
        f s = Nothing
        g oc | oc `elem` [Icmpsb, Icbw, Imovsb] = Just Bits8
        g oc | oc `elem` [Icmpsw, Icwd, Imovsw] = Just Bits16
        g _ = Nothing

    sizeByte = case size of
        Bits8 -> 1
        Bits16 -> 2
    size_ = case size of
        Bits8  -> Pack fetch8 update8 al ah ax
        Bits16 -> Pack fetch16 update ax dx dxax

    update8 :: Operand -> Word8 -> Machine ()
    update8 x y = case x of
        Reg r -> reg8 r .= y
        Mem m -> compAddr m >>= \a -> heap8 a .= y
        x -> error $ "update8: " ++ show x

    update :: Operand -> Word16 -> Machine ()
    update x y = case x of
        Reg r -> reg r .= y
        Mem m -> compAddr m >>= \a -> heap16 a .= y
        x -> error $ "update: " ++ show x
        
data Pack where
    Pack :: forall a . (FiniteBits a, Integral a, AsSigned a, Integral (X2 a), AsSigned (X2 a))
        => (Operand -> Machine a)
        -> (Operand -> a -> Machine ())
        -> (Lens' MachineState a)
        -> (Lens' MachineState a)
        -> (Lens' MachineState (X2 a))
        -> Pack

type family X2 s :: *
type instance X2 Word8 = Word16
type instance X2 Word16 = Word32

--------------------------------------------------------------------------------

infixl 9 @.
m @. i = push_ i >> m

tests = do
    quickCheck $ \i -> eval (push i >> call "sin") == sin_fast i
    quickCheck $ \i -> eval (push i >> call "cos") == cos_fast i

    quickCheck $ \i j -> eval (call "polarAngle" @. i @. j) == fromIntegral (polarAngle (fromIntegral' i) (fromIntegral' j))
    quickCheck $ \i j -> eval (call "polarRadius2D" @. i @. j) == fromIntegral (polarRadius2D (fromIntegral' i) (fromIntegral' j))
--    q3d

q3d = quickCheck $ \i_ j_ k_ -> let
        i = fromIntegral (i_ :: Int8)
        j = fromIntegral (j_ :: Int8)
        k = fromIntegral (k_ :: Int8)
        v = V.fromList [i,j,k] :: Vect
    in {-polarRadius2D (fromIntegral' i) (fromIntegral' j) == 0x8000
     || -} eval (call "polarRadius3D" @. v) == fromIntegral (polarRadius3D $ V.map fromIntegral' v)

qrp = quickCheck $ \i j ->
        eval (call "rectComparePoint" @. (V.fromList [i,j] :: Vect)) == 1


------------------------

fromIntegral' :: Word16 -> Int
fromIntegral' v = if v >= 0x8000 then fromIntegral v - 0x10000 else fromIntegral v

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

