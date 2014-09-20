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
    expUniverse' dag exp = exp : expUniverse dag exp

gpUniverse :: DAG -> Exp -> [Exp]
gpUniverse dag gp = gp : gpUniverseV dag ! toExpId dag gp

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
