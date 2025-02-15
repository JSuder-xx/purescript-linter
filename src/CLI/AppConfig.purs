module CLI.AppConfig where

import Prelude

import Control.Apply (lift2)
import Data.Argonaut (Json, fromObject, printJsonDecodeError, (.:))
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Combinators ((.:!))
import Data.Argonaut.Decode.Decoders (decodeJObject)
import Data.Argonaut.Encode.Encoders (encodeArray, encodeBoolean, encodeInt, encodeString)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either, note')
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (guard)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object, fromFoldable, fromHomogeneous)
import Foreign.Object as Object
import Linter.ModuleRule (ModuleIssueIdentifier, ModuleRule)
import Linter.ModuleRule as ModuleRule
import Node.Path as Path

type AppConfigProcessed = AppConfig' ModuleIssueIdentifier

type AppConfigRaw = AppConfig' (Object Json)

type AppConfig' rules =
  { hideSuccess :: Boolean
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
  , rules :: rules
  }

withCwd :: forall a. String -> AppConfig' a -> AppConfig' a
withCwd cwd appConfig = appConfig
  { projectRoots = appConfig.projectRoots <#> \root ->
      root { pathPrefix = Path.concat [ cwd, root.pathPrefix ] }
  }

encodeDefault :: Array ModuleRule -> Json
encodeDefault allModuleRules = fromObject $ fromHomogeneous
  { hideSuccess: encodeBoolean true
  , indentSpaces: encodeInt 2
  , projectRoots: encodeArray identity
      [ fromObject $ fromHomogeneous
          { pathPrefix: encodeString "src"
          , modulePrefix: encodeString ""
          }
      ]
  , ruleSets: encodeArray identity
      [ fromObject $ fromHomogeneous
          { globs: encodeArray encodeString [ "./src/**/*.purs" ]
          , rules: fromObject $ fromFoldable $ Array.sortWith fst $ allModuleRules <#> lift2 Tuple ModuleRule.name ModuleRule.defaultConfigJson
          }
      ]
  }

decode :: Json -> Either JsonDecodeError AppConfigRaw
decode = decodeJObject >=> \object -> do
  hideSuccess <- fromMaybe true <$> object .:! "hideSuccess"
  indentSpaces <- fromMaybe 2 <$> object .:! "indentSpaces"
  projectRoots <- fromMaybe [] <$> object .:! "projectRoots"
  ruleSetsRaw :: Array (Object Json) <- object .: "ruleSets"
  ruleSets <- traverse decodeRuleSet ruleSetsRaw
  pure { hideSuccess, ruleSets, indentSpaces, projectRoots }
  where
  decodeRuleSet :: Object Json -> Either JsonDecodeError (RuleSet (Object Json))
  decodeRuleSet object = do
    globs <- object .: "globs"
    rules <- object .: "rules"
    pure { globs, rules }

rawToProcessed :: Array ModuleRule -> AppConfigRaw -> Either String AppConfigProcessed
rawToProcessed allModuleRules raw = traverse processRuleSet raw.ruleSets
  <#>
    { hideSuccess: raw.hideSuccess
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
    <#> { globs: ruleSet.globs, rules: _ }

  foldRule :: ModuleIssueIdentifier -> String -> Json -> Either String ModuleIssueIdentifier
  foldRule accProducer ruleName ruleConfigJson = do
    rule <- note' (\_ -> "Unable to find rule named '" <> ruleName <> "'." <> possibleRuleName ruleName) $ Map.lookup ruleName ruleMap
    moduleIssueIdentifier <-
      lmap (\jsonDecodeError -> "Had trouble decoding the configuration for rule '" <> ruleName <> "': " <> printJsonDecodeError jsonDecodeError)
        $ ModuleRule.decodeMkModuleIssueIdentifier ruleConfigJson rule
    pure $ moduleIssueIdentifier config <> accProducer
