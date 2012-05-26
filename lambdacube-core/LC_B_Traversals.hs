module LC_B_Traversals where

import Data.List

import LC_U_APIType
import LC_U_DeBruijn

class HasExp a where
    expUniverse    :: a -> [Exp]

instance HasExp a => HasExp [a] where
    expUniverse a = concatMap expUniverse a

instance HasExp FragmentFilter where
    expUniverse (Filter f) = expUniverse f
    expUniverse _ = []

instance HasExp GeometryShader where
    expUniverse (GeometryShader _ _ _ a b c) = expUniverse a ++ expUniverse b ++ expUniverse c
    expUniverse _ = []

instance HasExp ExpFun where
    expUniverse (Body e)   = e : expUniverse e
    expUniverse (Lam f)    = expUniverse f

instance HasExp Exp where
    expUniverse exp = case exp of
        Tup _ l                 -> l ++ expUniverse l
        Prj _ _ e               -> e : expUniverse e
        Cond _ a b c            -> a : b : c : expUniverse a ++ expUniverse b ++ expUniverse c
        PrimApp _ _ a           -> a : expUniverse a
        VertexOut a b c         -> a : b : expUniverse a ++ expUniverse b ++ expUniverse c
        GeometryOut a b c d e f -> a : b : c : d : e : expUniverse a ++ expUniverse b ++ expUniverse c ++ expUniverse d ++ expUniverse e ++ expUniverse f
        FragmentOut a           -> a ++ expUniverse a
        FragmentOutDepth a b    -> a : b ++ expUniverse a ++ expUniverse b
        FragmentOutRastDepth a  -> a ++ expUniverse a
        _                       -> []

instance HasExp (Interpolated Exp) where
    expUniverse a  = case a of
        Flat a          -> a : expUniverse a
        Smooth a        -> a : expUniverse a
        NoPerspective a -> a : expUniverse a

instance HasExp GP where
    expUniverse gp = case gp of
        Transform a b           -> expUniverse a ++ expUniverse b
        Rasterize _ a b         -> expUniverse a ++ expUniverse b
        Accumulate _ a b c _    -> expUniverse a ++ expUniverse b ++ expUniverse c
        PrjFrameBuffer _ _ a    -> expUniverse a
        PrjImage _ _ a          -> expUniverse a
        _                       -> []

gpUniverse :: GP -> [GP]
gpUniverse gp = gp : case gp of
    Transform _ a           -> gpUniverse a
    Rasterize _ _ a         -> gpUniverse a
    Accumulate _ _ _ a b    -> gpUniverse a ++ gpUniverse b
    PrjFrameBuffer _ _ a    -> gpUniverse a
    PrjImage _ _ a          -> gpUniverse a
    _                       -> []

-- includes the origin
gpUniverse' :: GP -> [GP]
gpUniverse' gp = gp : gpUniverse gp

-- includes the origin
expUniverse' :: Exp -> [Exp]
expUniverse' e = e : expUniverse e

findFrameBuffer :: GP -> GP
findFrameBuffer a = head $ dropWhile notFrameBuffer $ gpUniverse' a
  where
    notFrameBuffer (Accumulate {})  = False
    notFrameBuffer (FrameBuffer {}) = False
    notFrameBuffer _ = True

-- starts from a FrameBuffer GP and track the draw action chain until the FrameBuffer definition
renderChain :: GP -> [GP]
renderChain fb@(FrameBuffer {}) = [fb]
renderChain fb@(Accumulate _ _ _ _ a) = renderChain a ++ [fb]
renderChain _ = []


drawOperations :: GP -> [GP]
drawOperations fb@(FrameBuffer {}) = [fb]
drawOperations fb@(Accumulate _ _ _ a _) = fb : gpUniverse' a
drawOperations _ = []
