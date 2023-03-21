{-# LANGUAGE LambdaCase #-} -- enables (\case { ... }) syntax

-- | This executable program makes use of the Aeson JSON parsing library to parse a file of JSON
-- data given to it as an argument on the command line. This program performs no other action.
-- Aeson is written mostly in Haskell with a few parts written in C to eek-out the best performance
-- possible. To the author's knowledge, this has been the most widely-used JSON parser in the
-- Haskell software ecosystem for many years now.
--
-- https://hackage.haskell.org/package/hermes-json
-- https://github.com/simdjson/simdjson
--
module Main where

import ArgsMain (runWithMainArgs) -- imported from within this project

import qualified Data.Hermes     as JSON -- This is the JSON parser being tested

--------------------------------------------------

import Control.Applicative ((<|>)) -- alternative choice operator
import Control.Exception (evaluate) -- force strict evaluation semantics

import qualified Data.ByteString as Bytes
import qualified Data.Text       as Strict -- strict text strings

import qualified Data.Map.Strict as Map -- strict dictionary (map) data structures
import Data.Map.Strict (Map)

import qualified Data.Vector     as Vec -- strict boxed vectors
import Data.Vector (Vector)

----------------------------------------------------------------------------------------------------

-- | The Hermes library does not instantiate it's JSON data structure data types into the
-- 'Control.DeepSeq.NFData' typeclass. Furthermore, the @simdjson@ C++ library parses value on
-- demand, so itself may introduce some laziness into the parsing. To ensure parsing stores the
-- entire data structure in memory, strict data structure isomorphic to JSON objects is
-- defined. This may result in 2 copies of the data structure stored in memory, once in the C++
-- runtime, and once in the Haskell runtime.
data JTree
  = NULL
  | BOOL !Bool
  | NUMBER !Double
  | STRING !Strict.Text
  | ARRAY !(Vector JTree)
  | OBJECT !(Map Strict.Text JTree)

decodeJTree :: JSON.Value -> JSON.Decoder JTree
decodeJTree = loop where
  loop value =
    (ARRAY . Vec.fromList <$> JSON.list loop value) <|>
    (OBJECT . Map.fromList <$> JSON.objectAsKeyValues pure loop value) <|>
    (NUMBER <$> JSON.double value) <|>
    (STRING <$> JSON.text value) <|>
    (BOOL <$> JSON.bool value) <|>
    return NULL

---- The following code would be more efficient, but it requres the 'withType' function that is only
---- available from the hermes-json package version 0.3 or later. For whatever reason, the
---- hermes-json package does not have package dependencies compatible with the package dependencies
---- of this project (probably due to the use of Aeson)
--
--parseJSONValue :: JSON.Decoder JTree
--parseJSONValue = loop where
--  loop = JSON.withType $ \ case
--    JSON.VArray   -> JSON.withVector loop $ return . ARRAY
--    JSON.VObject  -> JSON.withObjectAsMap pure loop $ return . OBJECT
--    JSON.VNumber  -> JSON.withDouble $ return . NUMBER
--    JSON.VString  -> JSON.withText $ return . STRING
--    JSON.VBoolean -> JSON.withBool $ return . BOOL
--    JSON.VNull    -> return NULL

----------------------------------------------------------------------------------------------------

parseJSONFile :: FilePath -> IO ()
parseJSONFile filepath =
  -- Warning: 'Bytes.readFile' may have to buffer the entire file in memory.
  JSON.decodeEither decodeJTree <$>
  Bytes.readFile filepath >>= \ case
    Left   err  -> fail $ show err
    Right value -> evaluate value >> return ()
      -- Using 'evaluate' here ensures the enture 'Value' is in memory before returning.

main :: IO ()
main = runWithMainArgs parseJSONFile
