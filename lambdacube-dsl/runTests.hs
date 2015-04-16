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
import CompositionalLC hiding (test)
import Parser hiding (main, parseLC)

data ParseResult
  = ParseError String
  | TypeError String
  | TypedExp (Exp (Subst, Typing))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
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

  let checkErrorMsg msg n e = doesFileExist ef >>= \b -> case b of
        False -> writeFile ef e >> ok
        True -> do
            e' <- readFile ef
            if e == e' then ok else do
                putStrLn $ "Error message of reject test " ++ n ++ " has changed."
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
                        putStrLn "Accepted."
                    else putStrLn "Not Accepted."
        
        where
            ef = errorFileName n
            ok = putStrLn $ " * OK - " ++ n ++ " " ++ msg ++ " " ++ if verbose then e else []


  putStrLn $ "Catching errors (must get an error)"
  forM_ testToReject $ \n -> do
    result <- parseLC n
    case result of
      ParseError e -> checkErrorMsg "parse error" n e
      TypeError e -> checkErrorMsg "type error" n e
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
