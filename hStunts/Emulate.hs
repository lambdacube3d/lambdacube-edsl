{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
--{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Error
import Control.Lens as Lens
import Test.QuickCheck hiding ((.&.))
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

instance Extend Word8 where
    extend = fromIntegral
    type X2 Word8 = Word16
instance Extend Word16 where
    extend = fromIntegral
    type X2 Word16 = Word32
instance Extend Int8 where
    extend = fromIntegral
    type X2 Int8 = Int16
instance Extend Int16 where
    extend = fromIntegral
    type X2 Int16 = Int32

fromIntegral' :: (AsSigned a, Num b) => a -> b
fromIntegral' = fromIntegral . asSigned

combine2x8 :: Iso' (Word8, Word8) Word16
combine2x8 = iso (\(hi,lo) -> fromIntegral hi `shiftL` 8 .|. fromIntegral lo) (\d -> (fromIntegral $ d `shiftR` 8, fromIntegral d))

combine2x16 :: Iso' (Word16, Word16) Word32
combine2x16 = iso (\(hi,lo) -> fromIntegral hi `shiftL` 16 .|. fromIntegral lo) (\d -> (fromIntegral $ d `shiftR` 16, fromIntegral d))

high, low :: Lens' Word16 Word8
high = from combine2x8 . _1
low = from combine2x8 . _2

bit :: Bits a => Int -> Lens' a Bool
bit i = lens (`testBit` i) $ \x b -> if b then x `setBit` i else x `clearBit` i

highBit :: forall a . FiniteBits a => Lens' a Bool
highBit = lens (^. bit i) $ \x b -> bit i .~ b $ x
  where
    i = finiteBitSize (undefined :: a) - 1

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
{-
byteAt i tr m = indexed tr i a <&> \a' -> x S.>< a' <| y'
  where
    (x, y) = S.splitAt i m
    a S.:< y' = S.viewl y
-}
wordAt :: Int -> Lens' Mem Word16
wordAt i = (byteAt (i+1) `uComb` byteAt i) . combine2x8       -- could be faster

dwordAt :: Int -> Lens' Mem Word32
dwordAt i = (wordAt (i+2) `uComb` wordAt i) . combine2x16       -- could be faster

----------------

type Size = Int -- size in bytes

data Key
    = KReg Size Int
    | Flag Int
    | Flags
    | Heap Size Int
        deriving (Eq, Ord)

flagnames = "____oditsz_a_p_c"
reg8names = ["al","ah","dl","dh","bl","bh","cl","ch"]
reg16names = ["ax","dx","bx","cx", "si","di", "cs","ss","ds","es", "ip","sp","bp"]

combine (Heap s i: ks) | s `elem` [1,2] = run (i + s) ks
  where
    run i (Heap s' i': ks) | i==i' && s==s' = run (i + s) ks
    run i' ks = Heap (i'-i) i: combine ks
combine (k:ks) = k: combine ks
combine [] = []

instance Show Key where
    show (Heap _ i) = "[" ++ showHex' 5 i ++ "]"
    show (Flag i) = [flagnames !! (15-i)]
    show Flags = "flags"
    show (KReg 1 i) = reg8names !! i
    show (KReg 2 i) = reg16names !! (i `div` 2)
    show (KReg 4 0) = "ax:dx"

data Config_ = Config_
    { _numOfDisasmLines :: Int
    }

$(makeLenses ''Config_)

defConfig = Config_
    { _numOfDisasmLines = 30
    }

data MachineState = MachineState
    { _flags' :: Word16
    , _regs  :: Mem
    , _heap  :: Mem
    , _hist  :: Set.Set Key
    , _config :: Config_
    }

$(makeLenses ''MachineState)

steps = config . numOfDisasmLines

clearHist = hist .= Set.empty

type MachinePart a = Lens' MachineState a

addHist :: Key -> MachinePart a -> MachinePart a
addHist sh k = lens (^. k) $ \s a -> hist %~ Set.insert sh $ set k a s

flagLenses@[_,_,_,_,overflowF,directionF,interruptF,trapF,signF,zeroF,_,adjustF,_,parityF,_,carryF]
    = [ addHist (Flag i) $ flags' . bit i | i <- [15,14..0] ]
reg16Lenses@[ax,dx,bx,cx, si,di, cs,ss,ds,es, ip,sp,bp]
    = [ addHist (KReg 2 i) $ regs . wordAt i | i <- [0,2..24] ]
reg8Lenses@[al,ah,dl,dh,bl,bh,cl,ch]
    = [ addHist (KReg 1 i) $ regs . byteAt i | i <- [0..7] ]
dxax = addHist (KReg 4 0) $ regs . dwordAt 0
xx :: MachinePart Word16
xx = lens (const $ error "xx") $ \s _ -> s
flags = addHist Flags flags'

flagsl = flags . low        -- TODO: add hist?

heap8  i = addHist (Heap 1 i) $ heap . byteAt i
heap16 i = addHist (Heap 2 i) $ heap . wordAt i

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
                            $   zip (map (showHex' 4) [s ^. sp, s ^. sp + 2..0xffff] ++ repeat "####")
                                    (map ((showHex' 4) . (s ^.) . heap16) [sps, sps + 2.. S.length (s ^. heap) - 1] ++ repeat "####")
        , "Code: "
        ] ++ map (take 149 . ("  " ++)) (take' (s ^. config . numOfDisasmLines) $ coode s)
      where
        sps = segAddr (s ^. ss) (s ^. sp)
        showReg c k = c ++ ":" ++ showHex' 4 (s ^. k)

coode :: MachineState -> [String]
coode s = case x of
    Left e -> show e: []
    Right x -> (pad 14 (map toUpper $ mdHex x) ++ " "
        ++ pad 27 (mdAssembly x) ++ " " ++ unwords (map shKey $ combine $ Set.toList $ s' ^. hist)): case y of
        Left e -> show e: []
        Right () -> coode s'
 where
    (x, s_) = flip runState s $ runErrorT $ fetchInstr

    (y, s') = flip runState s_ $ runErrorT $ do
        clearHist
        exec_ $ either (error . show) mdInst $ x

    shKey k = case k of
        Heap 1 i  -> diff (sh 1) $ heap . byteAt i
        Heap 2 i  -> diff (sh 2) $ heap . wordAt i
        Heap n i  -> "[" ++ showHex' 5 i ++ "-" ++ showHex' 5 (i+n-1) ++ "]"
        Flag i    -> diff sf $ flags . bit i
        Flags     -> diff showFlags flags
        KReg 1 i  -> diff (sh 1) $ regs . byteAt i
        KReg 2 i  -> diff (sh 2) $ regs . wordAt i
        KReg 4 i  -> diff (sh 4) $ regs . dwordAt i
      where
        diff :: Eq a => (a -> String) -> Lens' MachineState a -> String
        diff f l = par (v == v') $ f v'
          where
            v = s ^. l
            v' = s' ^. l

        sh :: (Show a, Integral a) => Int -> a -> String
        sh i v = show k ++ ":" ++ showHex' (2*i) v
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

infixl 9 @.
m @. i = push_ i >> m

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
        h = S.fromList $ concatMap (\w -> [w ^. low, w ^. high]) $ V.toList v

{-
findRet n = do
    op <- step_
    case inOpcode $ mdInst op of
        Iretf | n == 0 -> return ()
        Iretf -> findRet (n-1)
        Icall -> findRet (n+1)
        _ -> findRet n

step 0 s = s
step n s = step (n-1) $ snd $ step_ s
-}

quotRem' :: Integral a => a -> a -> (a, a)
quotRem' a 0 = (0, 0)
quotRem' a b = quotRem a b

sizeByte_ i@Inst{..}
    | inOpcode `elem` [Icmpsb, Icbw, Imovsb, Istosb] = 1
    | inOpcode `elem` [Icmpsw, Icwd, Imovsw, Istosw] = 2
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
        RBP -> bp
        RSP -> sp
        RAX -> ax
        RBX -> bx
        RCX -> cx
        RDX -> dx
        RSI -> si
        RDI -> di
    RegSeg r -> case r of
        ES -> es
        DS -> ds
        SS -> ss
        CS -> cs
    RegIP -> ip
    RegNone -> immLens 0
    x -> error $ "reg: " ++ show x

compAddr :: Maybe Segment -> Memory -> Getter MachineState Int
compAddr segmentPrefix (Memory _ r RegNone 0 i)
    = reg r >- \x -> maybe (segOf r) (reg . RegSeg) segmentPrefix
                   . to (`segAddr_` (imm i + fromIntegral x))
compAddr _ m = error $ "compAddr: " ++ show m

operandB :: Maybe Segment -> Operand -> MachinePart Word8
operandB segmentPrefix x = case x of
    Reg r -> case r of
        Reg8 r L -> case r of
            RAX -> al
            RBX -> bl
            RCX -> cl
            RDX -> dl
        Reg8 r H -> case r of
            RAX -> ah
            RBX -> bh
            RCX -> ch
            RDX -> dh
    Mem m -> compAddr segmentPrefix m >- heap8
    Imm (Immediate Bits8 v) -> immLens $ fromIntegral v
    Hdis86.Const (Immediate Bits0 0) -> immLens 1 -- !!!
    _ -> error $ "operandB: " ++ show x

operandW :: Maybe Segment -> Operand -> MachinePart Word16
operandW segmentPrefix x = case x of
    Reg r -> reg r
    Mem m -> compAddr segmentPrefix m >- heap16
    Imm i -> immLens $ imm i
    Jump i -> ip >- \x -> immLens $ x + imm i
    _ -> error $ "fetch: " ++ show x

imm = fromIntegral . iValue

immLens c = lens (const c) $ \_ _ -> error "can't update immediate value"

fetchInstr :: Machine Metadata
fetchInstr = do
    ips <- liftM2 segAddr (use cs) (use ip)
    Just (md, _) <- disassembleOne disasmConfig . BS.pack . take 20 . seqToList . S.drop ips <$> use heap
    ip %= (+ fromIntegral (mdLength md))
    return md

disasmConfig = Config Intel Mode16 SyntaxIntel 0

step_ :: Machine Metadata
step_ = do
    md <- fetchInstr
    exec_ $ mdInst md
    return md

exec_ :: Instruction -> Machine ()
exec_ i@Inst{..}
    | Rep `elem` inPrefixes = cycle'
    | RepE `elem` inPrefixes = cycle $ use zeroF
    | RepNE `elem` inPrefixes = cycle $ not <$> use zeroF
    | otherwise = body
  where
    body = execRep $ i { inPrefixes = filter (not . rep) inPrefixes }

    cycle' = do
        c <- use cx
        replicateM_ (fromIntegral c) body
        cx .= 0
        -- zeroF .= True ?

    cycle cond = use cx >>= m >>= (cx .=)
      where
        m 0 = return 0
        m n = do
            let n' = n-1
            b <- cond
            if b then body >> m n' else return n'

    rep p = p `elem` [Rep, RepE, RepNE]

execRep :: Instruction -> Machine ()
execRep i@Inst{..} = case inOpcode of

    _ | inOpcode `elem` [Iaaa, Iaad, Iaam, Iaas, Idaa, Idas] -> error $ "step: not implemented: " ++ show i

      | inOpcode == Iret || inOpcode == Iretf -> do
        pop >>= (ip .=)
        when (inOpcode == Iretf) $ pop >>= (cs .=)
        case length $ inOperands of
            0 -> return ()
            1 -> op1w >>= (sp %=) . (+)

    Ipusha -> sequence_ [use r >>= push | r <- [ax,cx,dx,bx,sp,bp,si,di]]
    Ipopa  -> sequence_ [pop >>= (r .=) | r <- [di,si,bp,xx,bx,dx,cx,ax]]
    Ipushfw -> use flags >>= push
    Ipopfw  -> pop >>= (flags .=)

    Iclc  -> carryF     .= False
    Icmc  -> carryF     %= not
    Istc  -> carryF     .= True
    Icld  -> directionF .= False
    Istd  -> directionF .= True
    Icli  -> interruptF .= False
    Isti  -> interruptF .= True

    Inop  -> return ()
    Ihlt  -> throwError Halt

    Isahf -> use ah >>= (flagsl .=)
    Ixlatb -> do
        x <- use al
        (use . operandB segmentPrefix) (Mem $ Memory undefined (Reg16 RBX) RegNone 0 $ Immediate Bits16 $ fromIntegral x) >>= (al .=)

    Iinto -> do
        b <- use overflowF
        when b $ interrupt 4
    Iiretw -> do
        pop >>= (ip .=)
        pop >>= (cs .=)
        pop >>= (flags .=)

    Icall -> do
        let Ptr (Pointer seg (Immediate Bits16 v)) = op1
        use cs >>= push
        use ip >>= push
        cs .= fromIntegral seg
        ip .= fromIntegral v
    Ilea -> op2addr >>= setOp1 . fromIntegral
    Iles -> do
        ad <- op2addr
        use (heap16 ad) >>= setOp1
        use (heap16 $ ad + 2) >>= (es .=)
    Ilds -> do
        ad <- op2addr
        use (heap16 ad) >>= setOp1
        use (heap16 $ ad + 2) >>= (ds .=)
    Ijmp -> op1w >>= (ip .=)

    Ipush -> op1w >>= push
    Ipop  -> pop >>= setOp1

    Ija   -> condJump =<< (&&) <$> (not <$> use carryF) <*> (not <$> use zeroF)
    Ijae  -> condJump =<< not <$> use carryF
    Ijb   -> condJump =<< use carryF
    Ijbe  -> condJump =<< (||) <$> use carryF <*> use zeroF
    Ijcxz -> condJump =<< (== 0) <$> use cx
    Ijg   -> condJump =<< (&&) <$> ((==) <$> use signF <*> use overflowF) <*> (not <$> use zeroF)
    Ijge  -> condJump =<< (==) <$> use signF <*> use overflowF
    Ijl   -> condJump =<< (/=) <$> use signF <*> use overflowF
    Ijle  -> condJump =<< (||) <$> ((/=) <$> use signF <*> use overflowF) <*> use zeroF
    Ijno  -> condJump =<< not <$> use overflowF
    Ijnp  -> condJump =<< not <$> use parityF
    Ijns  -> condJump =<< not <$> use signF
    Ijnz  -> condJump =<< not <$> use zeroF
    Ijo   -> condJump =<< use overflowF
    Ijp   -> condJump =<< use parityF
    Ijs   -> condJump =<< use signF
    Ijz   -> condJump =<< use zeroF
    Iloop   -> loop $ return True
    Iloope  -> loop $ use zeroF
    Iloopnz -> loop $ not <$> use zeroF

    Iint -> use (operandB segmentPrefix op1) >>= interrupt

    _ -> case sizeByte of
        1 -> withSize (operandB segmentPrefix) al ah ax
        2 -> withSize (operandW segmentPrefix) ax dx dxax

  where
    withSize :: forall a . (AsSigned a, AsSigned (X2 a), X2 (Signed a) ~ Signed (X2 a))
        => (Operand -> MachinePart a)
        -> MachinePart a
        -> MachinePart a
        -> MachinePart (X2 a)
        -> Machine ()
    withSize tr alx ahd axd = case inOpcode of
        Imov -> op2v >>= setOp1v
        Ixchg -> do
            a <- op1v
            b <- op2v
            tr op1 .= b
            tr op2 .= a
        Inot -> op1v >>= setOp1v . complement

        Isal  -> shiftOp $ \_ x -> (x ^. highBit, x `shiftL` 1)
        Ishl  -> shiftOp $ \_ x -> (x ^. highBit, x `shiftL` 1)
        Ircl  -> shiftOp $ \c x -> (x ^. highBit, bit 0 .~ c $ x `shiftL` 1)
        Irol  -> shiftOp $ \_ x -> (x ^. highBit, x `rotateL` 1)
        Isar  -> shiftOp $ \_ x -> (x ^. bit 0, fromIntegral $ asSigned x `shiftR` 1)
        Ishr  -> shiftOp $ \_ x -> (x ^. bit 0, x `shiftR` 1)
        Ircr  -> shiftOp $ \c x -> (x ^. bit 0, highBit .~ c $ x `shiftR` 1)
        Iror  -> shiftOp $ \_ x -> (x ^. bit 0, x `rotateR` 1)

        Ineg  -> twoOp_ True (flip (-)) (tr op1) (immLens 0)
        Idec  -> twoOp_ True (+) (tr op1) (immLens (-1))
        Iinc  -> twoOp_ True (+) (tr op1) (immLens 1)
        Iadd  -> twoOp True (+)
        Iadc  -> do
            c <- fromEnum <$> use carryF
            twoOp True $ \a b -> a + b + fromIntegral c
        Iand  -> twoOp True  (.&.)
        Itest -> twoOp False (.&.)
        Icmp  -> twoOp False (-)
        Ior   -> twoOp True  (.|.)
        Isbb  -> do
            c <- fromEnum <$> use carryF
            twoOp True $ \a b -> a - b - fromIntegral c
        Isub  -> twoOp True (-)
        Ixor  -> twoOp True xor

        Idiv  -> divide id id
        Iidiv -> divide asSigned asSigned
        Imul  -> multiply id
        Iimul -> multiply asSigned

        _ | inOpcode == Istosb || inOpcode == Istosw -> do
            use alx >>= (di' .=)
            adjustIndex di
          | inOpcode == Ilodsb || inOpcode == Ilodsw -> do
            use si' >>= (alx .=)
            adjustIndex si
          | inOpcode == Icmpsb || inOpcode == Icmpsw -> do
            twoOp_ False (-) di' si'
            adjustIndex si
            adjustIndex di
          | inOpcode == Imovsb || inOpcode == Imovsw -> do
            use si' >>= (di' .=)
            adjustIndex si
            adjustIndex di
          | inOpcode == Icwd || inOpcode == Icbw -> do
            use alx >>= (axd .=) . fromIntegral'

        Iin -> do
            v <- use $ operandW segmentPrefix op2
            input v >>= setOp1v
        Iout -> do
            c <- op1w
            v <- op2v
            output' c (fromIntegral v :: Word16)

        _ -> error $ "fatal error step: " ++ show i

      where
        si', di' :: MachinePart a
        si' = tr $ memIndex RSI
        di' = tr $ memIndex RDI

        divide :: (Integral c, Integral (X2 c)) => (a -> c) -> (X2 a -> X2 c) -> Machine ()
        divide asSigned asSigned' = do
            (d, m) <- quotRem <$> (asSigned' <$> use axd) <*> (fromIntegral . asSigned <$> op1v)
            alx .= fromIntegral d
            ahd .= fromIntegral m

        multiply :: (Integral c) => (a -> c) -> Machine ()
        multiply asSigned = do
            x <- asSigned <$> use alx
            y <- asSigned <$> op1v
            let c = r' == r
                r = fromIntegral x * fromIntegral y :: Int
                r' = fromIntegral (x * y)
            axd .= fromIntegral r
            carryF .= c
            overflowF .= c

        op1v = use $ tr op1
        op2v = use $ tr op2
        setOp1v = (tr op1 .=)

        shiftOp :: (forall b . (AsSigned b) => Bool -> b -> (Bool, b)) -> Machine ()
        shiftOp op = do
            a <- op1v
            n <- fromIntegral <$> use (operandB segmentPrefix op2)
            c <- use carryF
            let (c'_, r_) = iterate (uncurry op) (c, a) !! (n - 1)
            let (c', r)   = op c'_ r_
            carryF .= c'
            overflowF .= (r ^. highBit /= r_ ^. highBit)
            setOp1v r

        twoOp :: Bool -> (forall b . (Integral b, FiniteBits b) => b -> b -> b) -> Machine ()
        twoOp store op = twoOp_ store op (tr op1) (tr op2)

        twoOp_ :: Bool -> (forall a . (Integral a, FiniteBits a) => a -> a -> a) -> MachinePart a -> MachinePart a -> Machine ()
        twoOp_ store op op1 op2 = do
            a <- use op1
            b <- use op2
            let r = op a b

            -- possible speedup: False for AND, OR, XOR
            when (inOpcode `notElem` [Idec, Iinc]) $
                carryF .= (fromIntegral r /= op (fromIntegral a :: Int) (fromIntegral b))
            overflowF .= (fromIntegral' r /= op (fromIntegral' a :: Int) (fromIntegral' b))

            zeroF     .= (r == 0)
            signF     .= r ^. highBit
            parityF   .= even (popCount r)
--          adjustF   .= undefined            -- ADC, ADD, CMP, CMPSB, CMPSW, DEC, ...

            when store $ op1 .= r

    loop cond = cx %= (+(-1)) >> (&&) <$> ((/= 0) <$> use cx) <*> cond >>= condJump

    memIndex r = Mem $ Memory undefined (Reg16 r) RegNone 0 $ Immediate Bits0 0

    adjustIndex i = do
        d <- use directionF
        i %= if d then (+(-sizeByte)) else (+sizeByte)

    sizeByte :: Word16
    sizeByte = fromIntegral $ sizeByte_ i

    condJump b = when b $ op1w >>= (ip .=)

    ~(op1: ~(op2:_)) = inOperands

    setOp1 = (operandW segmentPrefix op1 .=)
    op1w = use $ operandW segmentPrefix op1
    op2addr = case op2 of Mem m -> use $ compAddr segmentPrefix m

    segmentPrefix :: Maybe Segment
    segmentPrefix = listToMaybe [s | Seg s <- inPrefixes]

input v = do
    case v of
        _ -> throwError $ Err $ "input #" ++ showHex' 4 v

output' v x = do
    case v of
        _ -> throwError $ Err $ "output #" ++ showHex' 4 v ++ " 0x" ++ showHex' 4 x

interrupt 0x21 = do
    v <- use ah
    case v of
        0x25 -> do          -- Set Interrupt Vector
            v <- use al     -- interrupt vector number
            _ <- use ds     -- DS:DX = pointer to interrupt handler
            _ <- use dx
            return ()
        0x30 -> do
            al .= 0x04      -- major version number (2-5)
            ah .= 0x00      -- minor version number (in hundredths decimal)
            bh .= 0xff      -- 0xFF indicates MS-DOS
            do              -- 24 bit OEM serial number
                bl .= 0
                cx .= 0
        0x35 -> do          -- Get Interrupt Vector
            v <- use al     -- interrupt vector number
            es .= 0xe000    -- ES:BX = pointer to interrupt handler
            bx .= 0
        0x4a -> carryF .= False      -- unlimited memory available
        _    -> throwError $ Err $ "dos function #" ++ showHex' 2 v
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

prelude = S.fromList [0xf4] -- halt instruction

loadExe :: Word16 -> BS.ByteString -> MachineState
loadExe loadSegment gameExe = flip execState (MachineState undefined undefined undefined undefined defConfig) $ do
    clearHist
    heap .=
        prelude
            S.><
        S.replicate (paragraph loadSegment - S.length prelude) 0
            S.><
        S.fromList (BS.unpack $ relocate relocationTable loadSegment $ BS.drop headerSize gameExe)
            S.><
        S.replicate (paragraph additionalMemoryNeeded) (fromIntegral $ ord '?')
    regs .= S.fromList (replicate 26 0)
    ss .= ssInit + loadSegment
    sp .= spInit
    cs .= csInit + loadSegment
    ip .= ipInit
    flags .= 0x7202
    clearHist
  where
    (0x5a4d: bytesInLastPage: pagesInExecutable: relocationEntries:
     paragraphsInHeader: additionalMemoryNeeded: maxAdditionalMemoryNeeded: ssInit:
     spInit: checksum: ipInit: csInit:
     firstRelocationItemOffset: overlayNumber: headerLeft)
        = map combine $ everyNth 2 $ BS.unpack $ gameExe

    combine [low, high] = (high, low) ^. combine2x8

    headerSize = paragraph paragraphsInHeader
    executableSize = (fromIntegral pagesInExecutable `shiftL` 9)
            + if (bytesInLastPage > 0) then fromIntegral bytesInLastPage - 0x200 else 0 :: Int

    relocationTable = sort $ take (fromIntegral relocationEntries)
        $ map (\[a,b]-> segAddr b a) $ everyNth 2 $ drop (fromIntegral firstRelocationItemOffset `div` 2 - 14) headerLeft


relocate table loc exe = BS.concat $ fst: map add (bss ++ [last])
  where
    (last, fst: bss) = mapAccumL (flip go) exe $ zipWith (-) table $ 0: table

    go r (BS.splitAt r -> (xs, ys)) = (ys, xs)

    add (BS.uncons -> Just (x, BS.uncons -> Just (y, xs))) = BS.cons x' $ BS.cons y' xs
        where (y',x') = combine2x8 %~ (+ loc) $ (y,x)

--------------------------------------------------------------------------------

loadSegment = 0x120 -- can be arbitrary > 0

initState = unsafePerformIO $ loadExe loadSegment <$> BS.readFile "original/game.exe"

eval_ = flip evalState initState . runErrorT
exec = flip execState initState . runErrorT

eval :: Machine () -> Word16
eval s = either f undefined v
  where
    f Halt = s' ^. ax
    f e = error $ show e
    (v, s') = flip runState initState $ runErrorT $ s >> forever (void step_)

call :: String -> Machine ()
call name = do
    let Just (seg, ipInit) = lookup name symbols
    push 0x0000
    push 0x0000
    ds .= dataSegment
    cs .= segments V.! seg
    ip .= ipInit
  where
    dataSegment = 0x2b77 + loadSegment

    segments :: V.Vector Word16
    segments = V.fromList $ map (+ loadSegment) [0, 0, 0, 0, 0,  0, 0x15f2{-?-}, 0, 0, 0,  0, 0x1ea0{-?-}, 0x1ea2, initState ^. cs]

    symbols =
        [ (,) "sin"               (12, 0x3cbe)    -- 0x24f6e - hs
        , (,) "cos"               (12, 0x3d0c)
        , (,) "polarAngle"        (12, 0x2e)      -- 0x212de - hs
        , (,) "polarRadius2D"     (12, 0x1696)
        , (,) "polarRadius3D"     (11, 0x8)       -- 0x21298 - hs
        , (,) "rectComparePoint"  ( 6, 0xe) -- 0x187be - hs   -- x * 16 + y = 0x15ee2
        , (,) "main"              (13, initState ^. ip)
        ]


--------------------------------------------------------------------------------

tests = do
    quickCheck $ \i -> eval (push i >> call "sin") == sin_fast i
    quickCheck $ \i -> eval (push i >> call "cos") == cos_fast i

    quickCheck $ \i j -> eval (call "polarAngle" @. (i :: Word16) @. (j :: Word16)) == fromIntegral (polarAngle (fromIntegral' i) (fromIntegral' j))
    quickCheck $ \i j -> eval (call "polarRadius2D" @. (i :: Word16) @. (j :: Word16)) == fromIntegral (polarRadius2D (fromIntegral' i) (fromIntegral' j))
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

