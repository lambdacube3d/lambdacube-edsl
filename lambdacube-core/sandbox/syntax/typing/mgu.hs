import Typing.Repr
import Typing.Infer

import Text.PrettyPrint.Leijen

import qualified Data.Map as Map
import Data.Monoid
import Prelude hiding (mapM)

testInfer m = runInfer m (TyEnv tyEnv) (PolyEnv mempty)
  where
    tyEnv = Map.fromList [ ("Nil", tyList a)
                         , ("Con", a ~> tyList a ~> tyList a)
                         ]

    tyList t = TyApp (TyCon "[]") t
    a = TyVar "a"

-- (inactivate-input-method)
-- (activate-input-method "Agda")

foo = testInfer $ inferExpr $ case map2 of
    Match [pat] e -> ELam pat e
      -- inferDefs (Defs [[defMap]])
  where
    defMap = DefFun "map" [map1, map2]

    -- Con x xs -> Con (f x) (map f xs)
    map1 = Match [PCon "Con" [PVar "x", PVar "xs"]] $
           EApp (EApp (ECon "Con") (EApp (EVar "f") (EVar "x")))
                (EApp (EApp (EVar "map") (EVar "f")) (EVar "xs"))
    -- Nil -> Nil
    map2 = Match [PCon "Nil" []] $
           ECon "Nil"
