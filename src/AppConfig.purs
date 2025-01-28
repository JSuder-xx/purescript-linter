module AppConfig where

import Prelude

import Control.Apply (lift2)
import Data.Argonaut (Json, JsonDecodeError(..), fromObject, (.:))
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Combinators ((.:!))
import Data.Argonaut.Decode.Decoders (decodeJObject)
import Data.Argonaut.Encode.Encoders (encodeArray, encodeBoolean, encodeString)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object, fromFoldable, fromHomogeneous)
import Foreign.Object as Object
import Rule (ModuleIssueIdentifier, Rule)
import Rule as Rule

type AppConfig =
  { hideSuccess :: Boolean
  , ruleSets :: Array RuleSet
  , indentSpaces :: Int
  }

type RuleSet =
  { globs :: Array String
  , moduleIssueIdentifier :: ModuleIssueIdentifier
  }

encodeDefault :: Array Rule -> Json
encodeDefault knownRules = fromObject $ fromHomogeneous
  { hideSuccess: encodeBoolean true
  , ruleSets: encodeArray identity
      [ fromObject $ fromHomogeneous
          { globs: encodeArray encodeString [ "./src/**/*.purs" ]
          , rules: fromObject $ fromFoldable $ Array.sortWith fst $ knownRules <#> lift2 Tuple Rule.name Rule.defaultConfigJson
          }
      ]
  }

decode :: Array Rule -> Json -> Either JsonDecodeError AppConfig
decode knownRules = decodeJObject >=> \object -> do
  hideSuccess <- fromMaybe true <$> object .:! "hideSuccess"
  indentSpaces <- fromMaybe 2 <$> object .:! "indentSpaces"
  ruleSetsRaw :: Array (Object Json) <- object .: "ruleSets"
  ruleSets <- traverse (decodeRuleSet indentSpaces) ruleSetsRaw
  pure { hideSuccess, ruleSets, indentSpaces }
  where
  ruleMap = knownRules <#> lift2 Tuple Rule.name identity # Map.fromFoldable

  decodeRuleSet :: Int -> Object Json -> Either JsonDecodeError RuleSet
  decodeRuleSet indentSpaces object = do
    globs <- object .: "globs"
    rules <- object .: "rules"
    moduleIssueIdentifier <- Object.foldM foldRule mempty rules
    pure { globs, moduleIssueIdentifier }
    where
    config = { indentSpaces }

    foldRule :: ModuleIssueIdentifier -> String -> Json -> Either JsonDecodeError ModuleIssueIdentifier
    foldRule accProducer ruleName ruleConfigJson = do
      rule <- note (Named ("Rule Named:" <> ruleName) MissingValue) $ Map.lookup ruleName ruleMap
      moduleIssueIdentifier <- Rule.decodeMkModuleIssueIdentifier ruleConfigJson rule
      pure $ moduleIssueIdentifier config <> accProducer
