{-# LANGUAGE DeriveDataTypeable #-}
module Args
    ( Args (..)
    , getArgs
    ) where

import Data.Version
import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.FilePath
import System.Console.CmdArgs.Implicit
import System.Exit (exitFailure)

import Paths_stunts

--------------------------------

data Args = Args
    { mediaPath     :: String
    , trkFile       :: String
    , carNum        :: Int
    , retroMode     :: Bool
    , verbose       :: Int
    }
        deriving (Show, Data, Typeable)

stuntsArgs :: Args
stuntsArgs = Args
    { mediaPath     = "STUNTS11.ZIP"    &= typFile     &= help "Game file, default is 'STUNTS11.ZIP'"
    , trkFile       = "zct114.trk"      &= typFile     &= help "Track file, default is 'zct114.trk'"
    , carNum        = 4                 &= typ "INT"   &= help "Car number, default is 4"
    , retroMode     = False                            &= help "Retro mode, default is off"
    , verbose       = 1                                &= help "Verbosity level of stdout debug info, default is 1"
    }  &= summary ("Haskell stunts " ++ showVersion version ++ ", (C) -2014 Csaba Hruska")
       &= program "stunts"

getArgs :: IO Args
getArgs = do
    x <- cmdArgs stuntsArgs
    gameOk <- doesFileExist $ mediaPath x
    unless gameOk $ do
        putStrLn "Missing game file! Please download the original game from"
        putStrLn "<http://downloads.pigsgrame.de/STUNTS11.ZIP> and move it to"
        putStrLn "the same folder as this executable."
        putStrLn "For reference, the above file should be 1077864 bytes."
        exitFailure
    return x
