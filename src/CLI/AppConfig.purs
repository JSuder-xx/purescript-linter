module CLI.AppConfig where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), encodeJson, printJsonDecodeError, (.:))
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Combinators ((.:!))
import Data.Argonaut.Decode.Decoders (decodeJObject, decodeString)
import Data.Argonaut.Encode.Encoders (encodeForeignObject, encodeString)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either, note')
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (guard)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object)
import Foreign.Object as Object
import Linter.ModuleRule (ModuleIssueIdentifier, ModuleRule)
import Linter.ModuleRule as ModuleRule
import Node.Path as Path

type AppConfigProcessed = AppConfig' ModuleIssueIdentifier

type AppConfigRaw = AppConfig' (Object Json)

type AppConfig' rules =
  { verbosity :: Verbosity
  , ruleSets :: Array (RuleSet rules)
  , indentSpaces :: Int
  , projectRoots :: Array ProjectRoot
  }

type ProjectRoot =
  { pathPrefix :: String
  , modulePrefix :: String
  }

type RuleSet rules =
  { globs :: Array String
  , ignoreGlobs :: Array String
  , rules :: rules
  }

data Verbosity
  -- | Only errors are shown.
  = Quiet
  -- | Shows an abbrieved progress message while running, any encountered errors, and a summary at the end.
  | Brief
  -- | Shows detailed progress messages, any encountered errors, and a summary at the end.
  | Verbose

derive instance Eq Verbosity
derive instance Ord Verbosity

instance EncodeJson Verbosity where
  encodeJson = case _ of
    Quiet -> encodeString "Quiet"
    Brief -> encodeString "Brief"
    Verbose -> encodeString "Verbose"

instance DecodeJson Verbosity where
  decodeJson = decodeString >=> String.trim >>> String.toLower >>> case _ of
    "quiet" -> pure Quiet
    "brief" -> pure Brief
    "verbose" -> pure Verbose
    x -> throwError $ TypeMismatch x

showProgress :: Verbosity -> Boolean
showProgress = case _ of
  Quiet -> false
  Brief -> true
  Verbose -> true

withCwd :: forall a. String -> AppConfig' a -> AppConfig' a
withCwd cwd appConfig = appConfig
  { projectRoots = appConfig.projectRoots <#> \root ->
      root { pathPrefix = Path.concat [ cwd, root.pathPrefix ] }
  }

defaultAppConfg :: Array ModuleRule -> AppConfigRaw
defaultAppConfg allModuleRules =
  { verbosity: Brief
  , indentSpaces: 2
  , projectRoots:
      [ { pathPrefix: "src"
        , modulePrefix: ""
        }
      ]
  , ruleSets:
      [ { globs: [ "./src/**/*.purs" ]
        , ignoreGlobs: []
        , rules: Object.fromFoldable $ Array.sortWith fst $ allModuleRules <#> lift2 Tuple ModuleRule.name ModuleRule.defaultConfigJson
        }
      ]
  }

rulesSchema :: Array ModuleRule -> Json
rulesSchema rules = encodeJson
  { type: "object"
  , additionalProperties: false
  , properties: Object.fromFoldable $ ruleTuple <$> rules
  , description: "An object where the keys are the rule names and the values are the configuration"
  }
  where
  ruleTuple :: ModuleRule -> Tuple String Json
  ruleTuple = lift2 Tuple ModuleRule.name $ encodeForeignObject identity <<< ModuleRule.configJsonSchema

decode :: Json -> Either JsonDecodeError AppConfigRaw
decode = decodeJObject >=> \object -> do
  verbosity <- fromMaybe Brief <$> object .:! "verbosity"
  indentSpaces <- fromMaybe 2 <$> object .:! "indentSpaces"
  projectRoots <- fromMaybe [] <$> object .:! "projectRoots"
  ruleSetsRaw :: Array (Object Json) <- object .: "ruleSets"
  ruleSets <- traverse decodeRuleSet ruleSetsRaw
  pure { verbosity, ruleSets, indentSpaces, projectRoots }
  where
  decodeRuleSet :: Object Json -> Either JsonDecodeError (RuleSet (Object Json))
  decodeRuleSet object = do
    globs <- object .: "globs"
    ignoreGlobs <- fromMaybe [] <$> object .:! "ignoreGlobs"
    rules <- object .: "rules"
    pure { globs, ignoreGlobs, rules }

rawToProcessed :: Array ModuleRule -> AppConfigRaw -> Either String AppConfigProcessed
rawToProcessed allModuleRules raw = traverse processRuleSet raw.ruleSets
  <#>
    { verbosity: raw.verbosity
    , ruleSets: _
    , indentSpaces: raw.indentSpaces
    , projectRoots: raw.projectRoots
    }
  where
  ruleMap = allModuleRules <#> lift2 Tuple ModuleRule.name identity # Map.fromFoldable
  ruleNames = allModuleRules <#> ModuleRule.name
  config = { indentSpaces: raw.indentSpaces }

  possibleRuleName =
    String.take 1
      >>> (\firstChar -> Array.filter (\ruleName -> (String.take 1 ruleName) == firstChar) ruleNames)
      >>> \possibleRules -> guard (not Array.null possibleRules) $ "\nDid you mean any of these rules?\n\t" <> String.joinWith "\n\t" possibleRules

  processRuleSet :: RuleSet (Object Json) -> Either String (RuleSet ModuleIssueIdentifier)
  processRuleSet ruleSet = Object.foldM foldRule mempty ruleSet.rules
    <#> { globs: ruleSet.globs, ignoreGlobs: ruleSet.ignoreGlobs, rules: _ }

  foldRule :: ModuleIssueIdentifier -> String -> Json -> Either String ModuleIssueIdentifier
  foldRule accProducer ruleName ruleConfigJson = do
    rule <- note' (\_ -> "Unable to find rule named '" <> ruleName <> "'." <> possibleRuleName ruleName) $ Map.lookup ruleName ruleMap
    moduleIssueIdentifier <-
      lmap (\jsonDecodeError -> "Had trouble decoding the configuration for rule '" <> ruleName <> "': " <> printJsonDecodeError jsonDecodeError)
        $ ModuleRule.decodeMkModuleIssueIdentifier ruleConfigJson rule
    pure $ moduleIssueIdentifier config <> accProducer
