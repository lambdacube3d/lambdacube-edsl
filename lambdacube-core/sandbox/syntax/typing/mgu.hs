import Typing.Repr
import Typing.Infer

import Text.PrettyPrint.Leijen

import Data.Map (Map)
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

prettyVarMap :: Map Var Typing -> Doc
prettyVarMap = vcat . map (uncurry prettyVar) . Map.toList
  where
    prettyTyping (m, τ) = pretty m <+> text "⊢" <+> pretty τ
    prettyVar x typing = text x <+> text "∷" <+> prettyTyping typing

main = print . prettyVarMap $ foo

foo = testInfer $ inferDefs $ Defs [[defMap]]
  where
    defMap = DefFun "map" [map1, map2]

    -- Con x xs -> Con (f x) (map f xs)
    map1 = Match [PVar "f", PCon "Con" [PVar "x", PVar "xs"]] $
           EApp (EApp (ECon "Con") (EApp (EVar "f") (EVar "x")))
                (EApp (EApp (EVar "map") (EVar "f")) (EVar "xs"))
    -- Nil -> Nil
    map2 = Match [PVar "f", PCon "Nil" []] $
           ECon "Nil"
