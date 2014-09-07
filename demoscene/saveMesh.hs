{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.Environment
import ThriftUtils
import LambdaCube.GL.Mesh

import qualified Data.ByteString.Char8 as SB

main :: IO ()
main = do
    l <- getArgs
    p <- protocol' "192.168.0.100"
    forM_ (map SB.pack l) $ \n -> do
        m <- remoteMesh p n
        saveMesh (SB.append n ".lcmesh") m
--        m' <- loadMesh (SB.append n ".lcmesh")
--        print $ m == m'
