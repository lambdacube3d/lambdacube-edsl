{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Applicative
import Control.Monad

import Text.Trifecta (Result (..))

import System.Environment
import System.Directory
import System.FilePath

import Type
import CompositionalLC hiding (test)
import ParseTrifectaLC

data ParseResult
  = ParseError String
  | TypeError String
  | TypedExp (Exp Typing)

main :: IO ()
main = do
  let acceptPath = "./tests/accept/"
      rejectPath = "./tests/reject/"

  samplesToAccept <- getArgs
  (testToAccept,testToReject) <- case samplesToAccept of
    [] -> do
      toAccept <- filter (\n -> ".lc" == takeExtension n) <$> getDirectoryContents acceptPath
      toReject <- filter (\n -> ".lc" == takeExtension n) <$> getDirectoryContents rejectPath
      return (map (acceptPath ++) toAccept,map (rejectPath ++) toReject)
    _ -> return (samplesToAccept,[])

  putStrLn $ "Catching errors (must get an error)"
  forM_ testToReject $ \n -> do
    putStr $ " " ++ n
    result <- parseLC n
    case result of
      ParseError e -> putStrLn $ " OK - got parse error: " ++ e
      TypeError e -> putStrLn $ " OK - got type error: " ++ e
      TypedExp _ -> putStrLn " FAIL - failed to catch error"

  putStrLn $ "Checking valid pipelines"
  forM_ testToAccept $ \n -> do
    putStr $ " " ++ n
    result <- parseLC n
    case result of
      ParseError e -> putStrLn $ " FAIL - parse error: " ++ e
      TypeError e -> putStrLn $ " FAIL - type error: " ++ e
      TypedExp _ -> putStrLn " OK"

parseLC :: String -> IO ParseResult
parseLC fname = do
  (src, res) <- parseLC_ fname
  case res of
    Failure m -> do
      return (ParseError $ show m)
    Success e -> do
      case inference src e of
        Right t   -> do
          return (TypedExp t)
        Left m    -> do
          return (TypeError m)
