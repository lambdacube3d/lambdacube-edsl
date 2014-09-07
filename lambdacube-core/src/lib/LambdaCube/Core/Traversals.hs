module LambdaCube.Core.Traversals where

import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import LambdaCube.Core.Type
import LambdaCube.Core.DeBruijn hiding (expUniverse, gpUniverse)
import Data.Vector ((!),Vector,(//))

class HasExp a where
    expUniverse    :: DAG -> a -> [Exp]
    expUniverse'   :: DAG -> a -> [Exp] -- includes the origin

instance HasExp a => HasExp [a] where
    expUniverse dag a   = concatMap (expUniverse dag) a
    expUniverse' dag a  = concatMap (expUniverse' dag) a

instance HasExp ExpId where
    expUniverse dag e   = expUniverse dag $ toExp dag e
    expUniverse' dag e  = expUniverse' dag $ toExp dag e

instance HasExp Exp where
    expUniverse dag exp = expUniverseV dag ! toExpId dag exp
{-
    expUniverse dag exp = case exp of
        Lam f                   -> expUniverse dag f
        Body f                  -> let a = toExp dag f
                                   in a : expUniverse dag f
        Apply ia ib             -> let [a,b] = map (toExp dag) [ia,ib]
                                   in a : b : expUniverse dag a ++ expUniverse dag b
        Tup l                   -> let e = map (toExp dag) l
                                   in e ++ expUniverse dag e
        Prj _ i                 -> let e = toExp dag i 
                                   in e : expUniverse dag e
        Cond ia ib ic           -> let [a,b,c] = map (toExp dag) [ia,ib,ic]
                                   in a : b : c : expUniverse dag a ++ expUniverse dag b ++ expUniverse dag c
        PrimApp _ ia            -> let a = toExp dag ia
                                   in a : expUniverse dag a
        Loop ia ib ic id        -> let [a,b,c,d] = map (toExp dag) [ia,ib,ic,id]
                                   in a : b : c : d : expUniverse dag a ++ expUniverse dag b ++ expUniverse dag c ++ expUniverse dag d
        VertexOut ia ib ic id   -> let [a,b] = map (toExp dag) [ia,ib]
                                   in a : b : expUniverse dag a ++ expUniverse dag b ++ expUniverse dag ic ++ expUniverse dag id
        GeometryOut i j k l m   -> let [a,b,c] = map (toExp dag) [i,j,k]
                                   in a : b : c : expUniverse dag a ++ expUniverse dag b ++ expUniverse dag c ++ expUniverse dag l ++ expUniverse dag m
        FragmentOut i           -> let a = map (toExp dag) i
                                   in a ++ expUniverse dag a
        FragmentOutDepth i j    -> let a:b = map (toExp dag) (i:j)
                                   in a : b ++ expUniverse dag a ++ expUniverse dag b
        FragmentOutRastDepth i  -> let a = map (toExp dag) i
                                   in a ++ expUniverse dag a
        Transform a b           -> expUniverse dag a ++ expUniverse dag b
        Reassemble a b          -> expUniverse dag a ++ expUniverse dag b
        Rasterize _ a           -> expUniverse dag a
        Accumulate _ a b c _    -> expUniverse dag a ++ expUniverse dag b ++ expUniverse dag c
        PrjFrameBuffer _ _ a    -> expUniverse dag a
        PrjImage _ _ a          -> expUniverse dag a
        Filter f                -> expUniverse dag f
        Flat a                  -> toExp dag a : expUniverse dag a
        Smooth a                -> toExp dag a : expUniverse dag a
        NoPerspective a         -> toExp dag a : expUniverse dag a 
        GeometryShader _ _ _ a b c  -> expUniverse dag a ++ expUniverse dag b ++ expUniverse dag c
        _                       -> []
-}
    expUniverse' dag exp = exp : expUniverse dag exp

gpUniverse :: DAG -> Exp -> [Exp]
gpUniverse dag gp = gp : gpUniverseV dag ! toExpId dag gp
{-
gpUniverse dag gp = gp : case gp of
    Transform _ a           -> gpUniverse dag $ toExp dag a
    Reassemble _ a          -> gpUniverse dag $ toExp dag a
    Rasterize _ a           -> gpUniverse dag $ toExp dag a
    Accumulate _ _ _ a b    -> gpUniverse dag (toExp dag a) ++ gpUniverse dag (toExp dag b)
    PrjFrameBuffer _ _ a    -> gpUniverse dag $ toExp dag a
    PrjImage _ _ a          -> gpUniverse dag $ toExp dag a
    _                       -> []
-}
-- includes the origin
gpUniverse' :: DAG -> Exp -> [Exp]
gpUniverse' dag gp = gp : gpUniverse dag gp

findFrameBuffer :: DAG -> Exp -> Exp
findFrameBuffer dag a = case dropWhile notFrameBuffer $ gpUniverse' dag a of
    e:_ -> e
    _   -> error "findFrameBuffer: nil"
  where
    notFrameBuffer (Accumulate {})  = False
    notFrameBuffer (FrameBuffer {}) = False
    notFrameBuffer _ = True

-- starts from a FrameBuffer GP and track the draw action chain until the FrameBuffer definition
renderChain :: DAG -> Exp -> [Exp]
renderChain _ fb@(FrameBuffer {}) = [fb]
renderChain dag fb@(Accumulate _ _ _ _ a) = renderChain dag (toExp dag a) ++ [fb]
renderChain _ _ = []

drawOperations :: DAG -> Exp -> [Exp]
drawOperations dag fb@(FrameBuffer {}) = [fb]
drawOperations dag fb@(Accumulate _ _ _ a _) = fb : gpUniverse' dag (toExp dag a)
drawOperations _ _ = []
