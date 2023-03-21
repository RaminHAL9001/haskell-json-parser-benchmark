module ArgsMain where

import System.Environment (getArgs, getProgName) -- command line argument parsing

runWithMainArgs :: (FilePath -> IO ()) -> IO ()
runWithMainArgs theTestParser =
  getArgs >>= \ case
    [filepath] ->
      -- take exactly 1 command line argument, a path to a JSON file
      theTestParser filepath
    _ ->
      -- otherwise fail
      getProgName >>= \ progName -> 
      fail $ "\nUSAGE: " <> progName <> " <filepath> \n" <>
      "Takes exactly one file path as an argument.\n" <>
      "This file must contain JSON formatted data.\n"
