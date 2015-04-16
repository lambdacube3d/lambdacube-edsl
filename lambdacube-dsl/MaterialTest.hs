
import Text.Show.Pretty

import Parser

main = do
    x <- parseLC "Material.lc"
    case x of
        Right m -> putStrLn $ ppShow m
        Left e -> error e

