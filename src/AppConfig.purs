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
import Rule (LintProducer, Rule)
import Rule as Rule

type AppConfig =
  { hideSuccess :: Boolean
  , ruleSets :: Array RuleSet
  }

type RuleSet =
  { globs :: Array String
  , lintProducer :: LintProducer
  }

decode :: Array Rule -> Json -> Either JsonDecodeError AppConfig
decode knownRules = decodeJObject >=> \object -> do
  hideSuccess <- fromMaybe true <$> object .:! "hideSuccess"
  ruleSetsRaw :: Array (Object Json) <- object .: "ruleSets"
  ruleSets <- traverse decodeRuleSet ruleSetsRaw
  pure { hideSuccess, ruleSets }
  where
  ruleMap = knownRules <#> lift2 Tuple Rule.name identity # Map.fromFoldable

  decodeRuleSet :: Object Json -> Either JsonDecodeError RuleSet
  decodeRuleSet object = do
    globs <- object .: "globs"
    rules <- object .: "rules"
    lintProducer <- Object.foldM foldRule mempty rules
    pure { globs, lintProducer }

  foldRule :: LintProducer -> String -> Json -> Either JsonDecodeError LintProducer
  foldRule accProducer ruleName ruleConfigJson = do
    rule <- note (Named ("Rule Named:" <> ruleName) MissingValue) $ Map.lookup ruleName ruleMap
    lintProducer <- Rule.decodeLintProducer ruleConfigJson rule
    pure $ lintProducer <> accProducer
