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
module Emulate where

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

align n i = ((i + n - 1) `div` n) * n

assert _ True = return ()
assert m _ = error m

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

segmentOf :: Int -> Word16
segmentOf i = fromIntegral i `shiftR` 4

segAddr_ :: Word16 -> Int -> Int
segAddr_ s w = paragraph s + w

segAddr :: Word16 -> Word16 -> Int
segAddr s w = segAddr_ s $ fromIntegral w

----------------

type Mem = S.Seq Word8

--seqIndex s i | i >= S.length s = error $ "seqIndex: " ++ show i
seqIndex s i = S.index s i

byteAt :: Int -> Lens' Mem Word8
byteAt i = lens (`seqIndex` i) $ \x b -> S.update i b x          -- could be faster
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
compAddr segmentPrefix (Memory _ r r' 0 i)
    = reg r >- \x -> reg r' >- \y -> maybe (segOf r) (reg . RegSeg) segmentPrefix
                   . to (`segAddr` (imm i + x + y))
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

    _ | inOpcode `elem` [Iaaa, Iaad, Iaam, Iaas, Idaa, Idas] -> throwError $ Err $ "step: not implemented: " ++ show i
      | length inOperands > 2 -> throwError $ Err "more than 2 operands are not supported"

      | inOpcode == Iret || inOpcode == Iretf -> do
        pop >>= (ip .=)
        when (inOpcode == Iretf) $ pop >>= (cs .=)
        case length inOperands of
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
    segmentPrefix = case inPrefixes of
        [Seg s] -> Just s
        [] -> Nothing

input v = do
    case v of
        0x60 -> return 0  -- hack
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
interrupt 0x10 = do
    v <- use al
    case v of
        0x13 -> do      --hack
            bx .= 4
        v  -> throwError $ Err $ "interrupt #10,#" ++ showHex' 2 v
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

loadCom :: BS.ByteString -> MachineState
loadCom com = flip execState (MachineState undefined undefined undefined undefined defConfig) $ do
    clearHist
    heap .=
        prelude
            S.><
        S.replicate (paragraph loadSegment - S.length prelude) 0
            S.><
        S.fromList (BS.unpack com)
            S.><
        S.replicate (l' - l + stacksize + 2^16) 0

    regs .= S.fromList (replicate 26 0)
    cs .= 0
    ip .= 0x0100
    ax .= 0
    bx .= 0
    cx .= 1
    di .= 0xfffe
    si .= 0x0100
    flags .= 0x7202

    forM_ [0xfff4..0xffff] $ \i -> do
        heap8 (segAddr gs i) .= 1
    heap8 (segAddr gs 0x20cd) .= 1

    ss .= segmentOf l'
    sp .= fromIntegral stacksize
    heap16 4 .= 0 --0x20cd
    heap16 6 .= gs       -- segment

    clearHist
  where
    l = BS.length com + paragraph loadSegment
    l' = align 16 l
    gs = segmentOf $ l' + stacksize

    stacksize = 2^8 :: Int

    loadSegment = 0x10

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

