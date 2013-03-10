{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.Environment
import ThriftUtils
import LC_Mesh

import qualified Data.ByteString.Char8 as SB

main :: IO ()
main = do
    l <- getArgs
    forM_ (map SB.pack l) $ \n -> do
        m <- remoteMesh n
        saveMesh (SB.append n ".lcmesh") m
--        m' <- loadMesh (SB.append n ".lcmesh")
--        print $ m == m'
