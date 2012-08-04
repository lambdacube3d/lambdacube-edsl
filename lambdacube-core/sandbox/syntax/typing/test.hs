import Typing.Repr
import Typing.Infer
import Typing.MonoEnv

import Text.PrettyPrint.Leijen

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Prelude hiding (mapM)

testInfer m = runInfer m (TyEnv tyEnv) (PolyEnv polyEnv)
  where
    tyEnv = Map.fromList [ ("Nil", tyList a)
                         , ("Con", a ~> tyList a ~> tyList a)
                         , ("Tuple4", a ~> b ~> c ~> d ~> (foldl TyApp (TyCon "Tuple") [a, b, c, d]))
                         ]
    polyEnv = Map.fromList [ ("not", (MonoEnv mempty, TyCon "Bool" ~> TyCon "Bool"))
                           , ("succ", (MonoEnv mempty, TyCon "Int" ~> TyCon "Int"))
                           ]

    tyList t = TyApp (TyCon "[]") t
    (a:b:c:d:_) = map (TyVar . (:[])) ['a'..]

prettyVarMap :: Map Var Typing -> Doc
prettyVarMap = vcat . map (uncurry prettyVar) . Map.toList
  where
    prettyTyping (m, τ) = pretty m <+> text "⊢" <+> pretty τ
    prettyVar x typing = text x <+> text "∷" <+> prettyTyping typing

main = print . prettyVarMap $ foo

foo = testInfer $ inferDefs $ defs' -- Defs [ [DefVar "test" defs e ] ]
  where
    defs = Defs [ [defMap, defMup]
                , [defIdFun]
                , [defIdLam]
                , [defConst]
                , [defMapConst]
                ]

    defs' = Defs [ [DefFun "foo"
                    [ Match [PVar "x"] (Defs [[localDef]]) $ EVar "not" @@ EVar "x"
                    ]
                   ]
                 ]
      where
        localDef = DefVar "y" (Defs []) $ EVar "succ" @@ EVar "x"

    e = foldl EApp (ECon "Tuple4") . map EVar $ ["map", "mup", "const", "mapConst"]

    defMap = DefFun "map" [map1 True, map2]
    defMup = DefFun "mup" [map1 False, map2]

    -- Con x xs -> Con (f x) (map f xs)
    map1 mup = Match [PVar "f", PCon "Con" [PVar "x", PVar "xs"]] (Defs []) $
               EApp (EApp (ECon "Con") (EApp (EVar "f") (EVar "x")))
                    (EApp (EApp (EVar recurse) (EVar "f")) (EVar "xs"))
      where
        recurse = if mup then "mup" else "map"
    -- Nil -> Nil
    map2 = Match [PVar "f", PCon "Nil" []] (Defs []) $
           ECon "Nil"

    defIdFun = DefFun "id" [Match [PVar "x"] (Defs []) $ EVar "x"]
    defIdLam = DefVar "id′" (Defs []) $ EApp (EVar "id") $ ELam (PVar "x") $ EVar "x"

    defConst = DefFun "const" [Match [PVar "x", PWildcard] (Defs []) $ EApp (EVar "id") (EVar "x")]

    defMapConst = DefVar "mapConst" (Defs []) $ EApp (EVar "map") (EVar "const")
