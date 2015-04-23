{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Data.List
import Control.Applicative
import Control.Monad

--import Text.Trifecta (Result (..))

import System.Environment
import System.Directory
import System.FilePath
import System.IO
--import Prelude

import Type
import CompositionalLC
import Core
import Parser

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  let acceptPath = "./tests/accept"
      rejectPath = "./tests/reject"
      errorFileName lc = dropExtension lc ++ ".error"

  args <- getArgs
  let (verboseFlags,samplesToAccept) = partition (== "-v") args
      verbose = verboseFlags /= []
  (testToAccept,testToReject) <- case samplesToAccept of
    [] -> do
      toAccept <- filter (\n -> ".lc" == takeExtension n) <$> getDirectoryContents acceptPath
      toReject <- filter (\n -> ".lc" == takeExtension n) <$> getDirectoryContents rejectPath
      return (map dropExtension toAccept,map dropExtension toReject)
    _ -> return (samplesToAccept,[])

  let checkErrorMsg n e = doesFileExist ef >>= \b -> case b of
        False -> writeFile ef e >> ok
        True -> do
            e' <- readFile ef
            if e == e' then ok else do
                putStrLn $ "Error message of " ++ n ++ " has changed."
                putStrLn "Old message: "
                putStrLn e'
                putStrLn "-------------------------------------------"
                putStrLn "New message: "
                putStrLn e
                putStrLn "-------------------------------------------"
                putStr "Accept new error message (y/n)? "
                c <- getChar
                if c `elem` ("yY\n" :: String) then do
                        writeFile ef e
                        putStrLn " Accepted."
                    else putStrLn " Not Accepted."
        where
            ef = errorFileName n
            ok = putStrLn $ " * OK - " ++ n ++ " " ++ if verbose then e else []


  putStrLn $ "Catching errors (must get an error)"
  forM_ testToReject $ \n -> do
    result <- typeCheckLC "./tests/reject" n
    case result of
      Left e -> checkErrorMsg n e
      _ -> putStrLn $ " # FAIL - " ++ n ++ " failed to catch error"

  putStrLn $ "Checking valid pipelines"
  forM_ testToAccept $ \n -> do
    result <- either Left (Right . fmap (toCore mempty) . getMain) <$> typeCheckLC "./tests/accept" n
    putStrLn $ case result of
      Left e -> " # FAIL - " ++ n ++ "\n" ++ e
      Right (Left _) -> " * OK (no main) - " ++ n
      Right (Right x) -> {- length (show x) `seq` -} (" * OK - " ++ n)

