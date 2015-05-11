{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Pretty
    ( module Pretty
    , Doc
    , (<+>), (</>), (<$$>)
    , hsep, hcat, vcat
    , punctuate
    , tupled, braces
    , text, int, integer, double, char
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint.Compact

--------------------------------------------------------------------------------

--instance IsString Doc where fromString = text

class PShow a where
    pShowPrec :: Int -> a -> Doc

pShow = pShowPrec 0
ppShow = show . pShow

ppShow' = show


{-
prec 0: no outer parens needed
prec 10: argument of a function


f x (g y)


-}

--------------------------------------------------------------------------------

pParens p x
    | p = parens x
    | otherwise = x

pOp i j k sep p a b = pParens (p >= i) $ pShowPrec j a <+> sep <+> pShowPrec k b

pInfixl i = pOp i (i-1) i
pInfixr i = pOp i i (i-1)
pInfix  i = pOp i i i

pApp = pInfixl 10 mempty
pApps p x xs = pParens (p > 9) $ hsep $ pShowPrec 9 x: map (pShowPrec 10) xs

--------------------------------------------------------------------------------

instance (PShow a, PShow b) => PShow (a, b) where
    pShowPrec p (a, b) = tupled [pShow a, pShow b]

instance (PShow a, PShow b, PShow c) => PShow (a, b, c) where
    pShowPrec p (a, b, c) = tupled [pShow a, pShow b, pShow c]

instance PShow a => PShow [a] where
    pShowPrec p = brackets . sep . punctuate comma . map pShow

instance PShow a => PShow (Set a) where
    pShowPrec p = pShowPrec p . Set.toList

instance (PShow s, PShow a) => PShow (Map s a) where
    pShowPrec p = braces . vcat . map (\(k, t) -> pShow k <> colon <+> pShow t) . Map.toList

instance (PShow a, PShow b) => PShow (Either a b) where
    pShowPrec p = either (("Left" <+>) . pShow) (("Right" <+>) . pShow)

instance PShow Doc where
    pShowPrec p x = braces x

