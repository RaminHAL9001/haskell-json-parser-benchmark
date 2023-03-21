{-# LANGUAGE LambdaCase #-} -- enables (\case { ... }) syntax

-- | This executable program makes use of the Aeson JSON parsing
-- library to parse a file of JSON data given to it as an argument on
-- the command line. This program performs no other action.
--
-- Aeson is written mostly in Haskell with a few parts written in C to
-- eek-out the best performance possible. To the author's knowledge,
-- this has been the most widely-used JSON parser in the Haskell
-- software ecosystem for many years now.
--
-- https://hackage.haskell.org/package/aeson
--
module Main where

import ArgsMain (runWithMainArgs) -- imported from within this project

import Control.DeepSeq (deepseq) -- force strict evaluation semantics

import qualified Data.Aeson as JSON

import System.FilePath (FilePath)

----------------------------------------------------------------------------------------------------

parseJSONFile :: FilePath -> IO ()
parseJSONFile filepath =
  JSON.decodeFileStrict filepath >>= \ case
    Nothing    -> fail "unknown parsing error occurred"
    Just value -> deepseq (value :: JSON.Value) $ return ()
      -- We use 'deepseq' to force the parsed JSON data structure to
      -- exist in memory. Without this, the compiler will take
      -- advantage of Haskell lazy semantics to optimize away the
      -- unnecessary work of storing the entire parsed JSON data
      -- structure in memory, which it would otherwise do since this
      -- data structure is never used in any way.

main :: IO ()
main = runWithMainArgs parseJSONFile
