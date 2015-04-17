{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Data.List
import Control.Applicative
import Control.Monad

import Text.Trifecta (Result (..))

import System.Environment
import System.Directory
import System.FilePath
import System.IO

import Type
import CompositionalLC
import Parser

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  let acceptPath = "./tests/accept/"
      rejectPath = "./tests/reject/"
      errorFileName lc = dropExtension lc ++ ".error"

  args <- getArgs
  let (verboseFlags,samplesToAccept) = partition (== "-v") args
      verbose = verboseFlags /= []
  (testToAccept,testToReject) <- case samplesToAccept of
    [] -> do
      toAccept <- filter (\n -> ".lc" == takeExtension n) <$> getDirectoryContents acceptPath
      toReject <- filter (\n -> ".lc" == takeExtension n) <$> getDirectoryContents rejectPath
      return (map (acceptPath ++) toAccept,map (rejectPath ++) toReject)
    _ -> return (samplesToAccept,[])

  let checkErrorMsg n e = doesFileExist ef >>= \b -> case b of
        False -> writeFile ef e >> ok
        True -> do
            e' <- readFile ef
            if e == e' then ok else do
                putStrLn $ "Error message of " ++ n ++ " has changed."
                putStrLn "Old message: "
                putStrLn e
                putStrLn "-------------------------------------------"
                putStrLn "New message: "
                putStrLn e'
                putStrLn "-------------------------------------------"
                putStr "Accept new error message (y/n)? "
                c <- getChar
                if c `elem` "yY\n" then do
                        writeFile ef e
                        putStrLn " Accepted."
                    else putStrLn " Not Accepted."
        where
            ef = errorFileName n
            ok = putStrLn $ " * OK - " ++ n ++ " " ++ if verbose then e else []


  putStrLn $ "Catching errors (must get an error)"
  forM_ testToReject $ \n -> do
    result <- parseLC' n
    case result of
      Just e -> checkErrorMsg n e
      Nothing -> putStrLn $ " # FAIL - " ++ n ++ " failed to catch error"

  putStrLn $ "Checking valid pipelines"
  forM_ testToAccept $ \n -> do
    result <- parseLC' n
    putStrLn $ case result of
      Just e -> " # FAIL - " ++ n ++ "\n" ++ e
      Nothing -> " * OK - " ++ n

parseLC' :: String -> IO (Maybe String)
parseLC' fname = do
  res <- parseLC fname
  return $ case res of
    Left m -> Just m
    Right (src, e) -> case inference e of
        Right _   -> Nothing
        Left m    -> Just $ m src

