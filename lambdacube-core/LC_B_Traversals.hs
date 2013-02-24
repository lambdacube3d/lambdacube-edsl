module LC_B_Traversals where

import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import LC_U_APIType
import LC_U_DeBruijn

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
    expUniverse dag exp = case exp of
        Lam f                   -> expUniverse dag f
        Body f                  -> let a = toExp dag f
                                   in a : expUniverse dag f
        Tup l                   -> let e = map (toExp dag) l
                                   in e ++ expUniverse dag e
        Prj _ i                 -> let e = toExp dag i 
                                   in e : expUniverse dag e
        Cond ia ib ic           -> let [a,b,c] = map (toExp dag) [ia,ib,ic]
                                   in a : b : c : expUniverse dag a ++ expUniverse dag b ++ expUniverse dag c
        PrimApp _ ia            -> let a = toExp dag ia
                                   in a : expUniverse dag a
        VertexOut ia ib ic      -> let [a,b] = map (toExp dag) [ia,ib]
                                   in a : b : expUniverse dag a ++ expUniverse dag b ++ expUniverse dag ic
        GeometryOut i j k l m n -> let [a,b,c,d,e] = map (toExp dag) [i,j,k,l,m]
                                   in a : b : c : d : e : expUniverse dag a ++ expUniverse dag b ++ expUniverse dag c ++ expUniverse dag d ++ expUniverse dag e ++ expUniverse dag n
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

    expUniverse' dag exp = exp : expUniverse dag exp

gpUniverse :: DAG -> Exp -> [Exp]
gpUniverse dag gp = gp : case gp of
    Transform _ a           -> gpUniverse dag $ toExp dag a
    Reassemble _ a          -> gpUniverse dag $ toExp dag a
    Rasterize _ a           -> gpUniverse dag $ toExp dag a
    Accumulate _ _ _ a b    -> gpUniverse dag (toExp dag a) ++ gpUniverse dag (toExp dag b)
    PrjFrameBuffer _ _ a    -> gpUniverse dag $ toExp dag a
    PrjImage _ _ a          -> gpUniverse dag $ toExp dag a
    _                       -> []

universe :: (Exp -> ([ExpId],[ExpId])) -> DAG -> Exp -> [Exp]
universe visitNode dag oe = map (toExp dag) $ reverse result
  where
    oei = toExpId dag oe
    (result,_,_) = go ([],IntSet.empty,IntSet.singleton oei) oei
    go :: ([ExpId],IntSet,IntSet) -> ExpId -> ([ExpId],IntSet,IntSet)
    go (resultList,resultSet,visitedSet) i
        | IntSet.member i visitedSet    = (resultList,resultSet,visitedSet)
        | otherwise                     = let (res,visit)   = visitNode $ toExp dag i
                                              (resL,resS)   = foldl' add (resultList,resultSet) res
                                              add (rl,rs) v = case IntSet.member v rs of
                                                --True    -> (rl,rs)
                                                _   -> (v:rl,IntSet.insert v rs)
                                          in foldl' go (resL,resS,IntSet.insert i visitedSet) visit

new_gpUniverse :: DAG -> Exp -> [Exp]
new_gpUniverse = universe gpChildren

expChildren exp = case exp of
    Lam f                       -> ([],[f]) -- result, visit
    Body f                      -> ([f],[f])
    Tup l                       -> (l,l)
    Prj _ t                     -> ([t],[t])
    Cond a b c                  -> ([a,b,c],[a,b,c])
    PrimApp _ a                 -> ([a],[a])
    VertexOut a b c             -> ([a,b],a:b:c)
    GeometryOut i j k l m n     -> ([i,j,k,l,m],i:j:k:l:m:n)
    FragmentOut i               -> (i,i)
    FragmentOutDepth i j        -> (i:j,i:j)
    FragmentOutRastDepth i      -> (i,i)
    Transform a b               -> ([],[a,b])
    Reassemble a b              -> ([],[a,b])
    Rasterize _ a               -> ([],[a])
    Accumulate _ a b c _        -> ([],[a,b,c])
    PrjFrameBuffer _ _ a        -> ([],[a])
    PrjImage _ _ a              -> ([],[a])
    Filter f                    -> ([],[f])
    Flat a                      -> ([a],[a])
    Smooth a                    -> ([a],[a])
    NoPerspective a             -> ([a],[a])
    GeometryShader _ _ _ a b c  -> ([],[a,b,c])
    _                           -> ([],[])

gpChildren :: Exp -> ([ExpId],[ExpId])
gpChildren gp = case gp of
    Transform _ a           -> ([a],[a])
    Reassemble _ a          -> ([a],[a])
    Rasterize _ a           -> ([a],[a])
    Accumulate _ _ _ a b    -> ([a,b],[a,b])
    PrjFrameBuffer _ _ a    -> ([a],[a])
    PrjImage _ _ a          -> ([a],[a])
    _                       -> ([],[])

-- includes the origin
gpUniverse' :: DAG -> Exp -> [Exp]
gpUniverse' dag gp = gp : gpUniverse dag gp

findFrameBuffer :: DAG -> Exp -> Exp
findFrameBuffer dag a = head $ dropWhile notFrameBuffer $ gpUniverse' dag a
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
