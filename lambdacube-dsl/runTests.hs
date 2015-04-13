{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Data.List
import Control.Applicative
import Control.Monad

import Text.Trifecta (Result (..))

import System.Environment
import System.Directory
import System.FilePath

import Type
import CompositionalLC hiding (test)
--import ParseTrifectaLC
import Parser hiding (main, parseLC)

data ParseResult
  = ParseError String
  | TypeError String
  | TypedExp (Exp (Subst, Typing))

main :: IO ()
main = do
  let acceptPath = "./tests/accept/"
      rejectPath = "./tests/reject/"

  args <- getArgs
  let (verboseFlags,samplesToAccept) = partition (== "-v") args
      verbose = verboseFlags /= []
  (testToAccept,testToReject) <- case samplesToAccept of
    [] -> do
      toAccept <- filter (\n -> ".lc" == takeExtension n) <$> getDirectoryContents acceptPath
      toReject <- filter (\n -> ".lc" == takeExtension n) <$> getDirectoryContents rejectPath
      return (map (acceptPath ++) toAccept,map (rejectPath ++) toReject)
    _ -> return (samplesToAccept,[])

  putStrLn $ "Catching errors (must get an error)"
  forM_ testToReject $ \n -> do
    result <- parseLC n
    case result of
      ParseError e -> putStrLn $ " * OK - " ++ n ++ " parse error " ++ if verbose then e else []
      TypeError e -> putStrLn $ " * OK - " ++ n ++ " type error " ++ if verbose then e else []
      TypedExp _ -> putStrLn $ " # FAIL - " ++ n ++ " failed to catch error"

  putStrLn $ "Checking valid pipelines"
  forM_ testToAccept $ \n -> do
    result <- parseLC n
    case result of
      ParseError e -> putStrLn $ " # FAIL - " ++ n ++ " parse error: " ++ e
      TypeError e -> putStrLn $ " # FAIL - " ++ n ++ " type error: " ++ e
      TypedExp _ -> putStrLn $ " * OK - " ++ n

parseLC :: String -> IO ParseResult
parseLC fname = do
  (src, res) <- parseLC_ fname
  case res of
    Failure m -> do
      return (ParseError $ show m)
    Success e -> do
      case inference e of
        Right t   -> do
          return (TypedExp t)
        Left m    -> do
          return (TypeError $ m src)
