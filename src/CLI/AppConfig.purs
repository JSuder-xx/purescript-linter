module CLI.AppConfig where

import Prelude

import Control.Apply (lift2)
import Data.Argonaut (Json, JsonDecodeError(..), fromObject, (.:))
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Decode.Combinators ((.:!))
import Data.Argonaut.Decode.Decoders (decodeJObject)
import Data.Argonaut.Encode.Encoders (encodeArray, encodeBoolean, encodeInt, encodeString)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object, fromFoldable, fromHomogeneous)
import Foreign.Object as Object
import Linter.ModuleRule (ModuleIssueIdentifier, ModuleRule)
import Linter.ModuleRule as ModuleRule
import Node.Path as Path

type AppConfig =
  { hideSuccess :: Boolean
  , ruleSets :: Array RuleSet
  , indentSpaces :: Int
  , projectRoots :: Array ProjectRoot
  }

type ProjectRoot =
  { pathPrefix :: String
  , modulePrefix :: String
  }

type RuleSet =
  { globs :: Array String
  , moduleIssueIdentifier :: ModuleIssueIdentifier
  , verifyModuleNameFromRootPath' :: Maybe String
  }

withCwd :: String -> AppConfig -> AppConfig
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

decode :: Array ModuleRule -> Json -> Either JsonDecodeError AppConfig
decode allModuleRules = decodeJObject >=> \object -> do
  hideSuccess <- fromMaybe true <$> object .:! "hideSuccess"
  indentSpaces <- fromMaybe 2 <$> object .:! "indentSpaces"
  projectRoots <- fromMaybe [] <$> object .:! "projectRoots"
  ruleSetsRaw :: Array (Object Json) <- object .: "ruleSets"
  ruleSets <- traverse (decodeRuleSet indentSpaces) ruleSetsRaw
  pure { hideSuccess, ruleSets, indentSpaces, projectRoots }
  where
  ruleMap = allModuleRules <#> lift2 Tuple ModuleRule.name identity # Map.fromFoldable

  decodeRuleSet :: Int -> Object Json -> Either JsonDecodeError RuleSet
  decodeRuleSet indentSpaces object = do
    globs <- object .: "globs"
    rules <- object .: "rules"
    verifyModuleNameFromRootPath' <- object .:! "verifyModuleNameFromRootPath"
    moduleIssueIdentifier <- Object.foldM foldRule mempty rules
    pure { globs, moduleIssueIdentifier, verifyModuleNameFromRootPath' }
    where
    config = { indentSpaces }

    foldRule :: ModuleIssueIdentifier -> String -> Json -> Either JsonDecodeError ModuleIssueIdentifier
    foldRule accProducer ruleName ruleConfigJson = do
      rule <- note (Named ("Rule Named:" <> ruleName) MissingValue) $ Map.lookup ruleName ruleMap
      moduleIssueIdentifier <- ModuleRule.decodeMkModuleIssueIdentifier ruleConfigJson rule
      pure $ moduleIssueIdentifier config <> accProducer
