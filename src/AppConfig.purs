module AppConfig where

import Prelude

import Control.Apply (lift2)
import Data.Argonaut (Json, JsonDecodeError(..), (.:))
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Combinators ((.:!))
import Data.Argonaut.Decode.Decoders (decodeJObject)
import Data.Either (Either, note)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Linter (LintProducer, Linter)
import Linter as Linter

type AppConfig =
  { hideSuccess :: Boolean
  , ruleSets :: Array RuleSet
  }

type RuleSet =
  { globs :: Array String
  , linter :: LintProducer
  }

decode :: Array Linter -> Json -> Either JsonDecodeError AppConfig
decode knownLinters = decodeJObject >=> \object -> do
  hideSuccess <- fromMaybe true <$> object .:! "hideSuccess"
  ruleSetsRaw :: Array (Object Json) <- object .: "ruleSets"
  ruleSets <- traverse decodeRuleSet ruleSetsRaw
  pure { hideSuccess, ruleSets }
  where
  linterMap = knownLinters <#> lift2 Tuple Linter.name identity # Map.fromFoldable

  decodeRuleSet :: Object Json -> Either JsonDecodeError RuleSet
  decodeRuleSet object = do
    globs <- object .: "globs"
    rules <- object .: "rules"
    linter <- Object.foldM foldRule mempty rules
    pure { globs, linter }

  foldRule :: LintProducer -> String -> Json -> Either JsonDecodeError LintProducer
  foldRule accProducer ruleName ruleConfigJson = do
    linter <- note (Named ("Rule Named:" <> ruleName) MissingValue) $ Map.lookup ruleName linterMap
    lintProducer <- Linter.decodeLintProducer ruleConfigJson linter
    pure $ lintProducer <> accProducer
