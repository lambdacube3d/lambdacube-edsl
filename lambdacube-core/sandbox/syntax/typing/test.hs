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

foo = testInfer $ inferDefs $ Defs [ [defMap, defMup]
                                   , [defIdFun]
                                   , [defIdLam]
                                   , [defConst]
                                   , [defMapConst]
                                   ]
  where
    defMap = DefFun "map" [map1 True, map2]
    defMup = DefFun "mup" [map1 False, map2]

    -- Con x xs -> Con (f x) (map f xs)
    map1 mup = Match [PVar "f", PCon "Con" [PVar "x", PVar "xs"]] $
               EApp (EApp (ECon "Con") (EApp (EVar "f") (EVar "x")))
                    (EApp (EApp (EVar recurse) (EVar "f")) (EVar "xs"))
      where
        recurse = if mup then "mup" else "map"
    -- Nil -> Nil
    map2 = Match [PVar "f", PCon "Nil" []] $
           ECon "Nil"

    defIdFun = DefFun "id" [Match [PVar "x"] $ EVar "x"]
    defIdLam = DefVar "id′" $ EApp (EVar "id") $ ELam (PVar "x") $ EVar "x"

    defConst = DefFun "const" [Match [PVar "x", PWildcard] $ EApp (EVar "id") (EVar "x")]

    defMapConst = DefVar "mapConst" $ EApp (EVar "map") (EVar "const")
