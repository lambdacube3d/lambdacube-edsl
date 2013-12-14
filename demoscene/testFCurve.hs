{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Applicative
import System.Environment
import qualified Data.ByteString.Char8 as SB

import ThriftUtils
import FCurve

main :: IO ()
main = do
    [objName,dataPath,timeStr] <- getArgs
    p <- protocol' "192.168.0.100"
    fcurve <- remoteFCurve p (SB.pack objName) (SB.pack dataPath)
    print fcurve
    bvals <- read <$> readFile "curve.out" :: IO [(Float,Float,Float)]
    let i = [-100..300]
        vals = map (evaluate fcurve) i
        err = [a | a@(_,_,e) <- zipWith3 cmp i vals bvals, e > 0.0001]
        cmp i a@[a0,a2,a3] b@(b1,b2,b3) = (i,(a,b),sum $ map abs $ zipWith (-) a [b1,b2,b3])
    print "errors:"
    mapM_ print err
