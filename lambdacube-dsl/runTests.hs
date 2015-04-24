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
import Text.Show.Pretty

import Type
import Typecheck
import Core
import Parser
import Driver
import CoreToIR

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  let acceptPath = "./tests/accept"
      rejectPath = "./tests/reject"
      errorFileName lc = rejectPath </> (lc ++ ".error")
      okFileName lc = acceptPath </> (lc ++ ".res")

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
                putStrLn $ "Error message has changed."
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
            ok = putStrLn $ "OK" ++ if verbose then e else []


  putStrLn $ "Catching errors (must get an error)"
  forM_ testToReject $ \n -> do
    putStr $ " # " ++ n ++ " ... "
    result <- runMM "./tests/reject" $ typeCheckLC n
    case result of
      Left e -> checkErrorMsg n e
      _ -> putStrLn $ "\n!FAIL - failed to catch error"

  putStrLn $ "Checking valid pipelines"
  forM_ testToAccept $ \n -> do
    putStr $ " # " ++ n ++ " ... "
    result <- runMM "./tests/accept" $ fmap (compilePipeline . reduce mempty mempty . toCore mempty) <$> getDef_ n "main" (Just $ [] ==> TCon0 "Output")
    let ef = okFileName n
    case result of
      Left e -> putStrLn $ "\n!FAIL\n" ++ e
      Right Nothing -> putStrLn "OK (no main)"
      Right (Just x) -> writeFile ef (ppShow x) >> putStrLn "OK"

