module Data.String.Regex.Extra
  ( RegexJson(..)
  , exampleRegex
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either, hush)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Partial.Unsafe (unsafePartial)

newtype RegexJson = RegexJson Regex

derive instance Newtype RegexJson _

instance EncodeJson RegexJson where
  encodeJson = encodeJson <<< Regex.source <<< unwrap

instance DecodeJson RegexJson where
  decodeJson = decodeRegex <=< decodeJson

parseRegex :: String -> Either String RegexJson
parseRegex = map RegexJson <<< flip Regex.regex Regex.Flags.global

decodeRegex :: String -> Either JsonDecodeError RegexJson
decodeRegex = parseRegex >>> lmap TypeMismatch

exampleRegex :: String -> RegexJson
exampleRegex s = unsafePartial $ fromJust $ hush $ parseRegex s
