{-# LANGUAGE DeriveDataTypeable #-}
module Args
    ( Args (..)
    , getArgs
    ) where

import Data.Version
import System.Console.CmdArgs.Implicit

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

getArgs :: IO Args
getArgs = cmdArgs $ Args
    { mediaPath     = ""                &= typFile     &= help "Game file"
    , trkFile       = "zct114.trk"      &= typFile     &= help "Track file, default is 'zct114.trk'"
    , carNum        = 4                 &= typ "INT"   &= help "Car number, default is 4"
    , retroMode     = False                            &= help "Retro mode, default is off"
    , verbose       = 1                                &= help "Verbosity level of stdout debug info, default is 1"
    }  &= summary ("Haskell stunts " ++ showVersion version ++ ", (C) 2010-2014 Csaba Hruska and others")
       &= program "stunts"
