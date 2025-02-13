module Data.Matcher
  ( Matcher(..)
  , exampleRegex
  , matches
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either, hush)
import Data.Maybe (fromJust, maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Partial.Unsafe (unsafePartial)

data Matcher
  = MatchAll
  | MatchExact (Array String)
  | MatchRegex Regex
  | MatchNotRegex Regex

instance EncodeJson Matcher where
  encodeJson = case _ of
    MatchAll -> encodeJson "All"
    MatchExact arr -> encodeJson arr
    MatchRegex regex -> encodeJson $ Regex.source regex
    MatchNotRegex regex -> encodeJson $ "NOT " <> Regex.source regex

instance DecodeJson Matcher where
  decodeJson json =
    decodeArray json <|> decodeString json
    where
    decodeArray = decodeJson >>> map MatchExact
    decodeString json' = do
      s <- decodeJson json'
      if (s == "All") then pure MatchAll
      else String.stripPrefix (Pattern "NOT ") s
        # maybe
            (MatchRegex <$> decodeRegex s)
            (map MatchNotRegex <<< decodeRegex)

parseRegex :: String -> Either String Regex
parseRegex = flip Regex.regex Regex.Flags.global

decodeRegex :: String -> Either JsonDecodeError Regex
decodeRegex = parseRegex >>> lmap TypeMismatch

exampleRegex :: String -> Regex
exampleRegex s = unsafePartial $ fromJust $ hush $ parseRegex s

matches :: String -> Matcher -> Boolean
matches _ MatchAll = true
matches s (MatchExact arr) = Array.elem s arr
matches s (MatchRegex regex) = Regex.test regex s
matches s (MatchNotRegex regex) = not Regex.test regex s
