module Linter.ModuleRules.Style.NamingPatterns where

import Prelude

import Data.Argonaut (Json, encodeJson)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Foldable (foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.String.Regex as Regex
import Data.String.Regex.Extra (RegexJson(..), exampleRegex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Linter.ModuleRule (ModuleRule, RuleCategory(..), mkModuleRule, typeIssueIdentifier)
import PureScript.CST.Separated as Separated
import PureScript.CST.Types (Label(..), Labeled(..), Name(..), Proper(..), QualifiedName(..), Row(..), Type(..), Wrapped(..))

fieldsOfContainerTypes :: ModuleRule
fieldsOfContainerTypes = mkModuleRule
  { name: "NamingPatterns.Fields.OfContainerTypes"
  , description:
      """Enforces that names of record fields of "container" types follow a given naming pattern.

For example, some engineers like to prefix or suffix `Maybe` values to differentiate between values that must be unwrapped.

Lenses are often prefixed with an `_` when declared at a top level ex. `_Just`, `_Newtype` but you may also want to follow that convention if putting an optic into a record or row (using the A/An version to avoid issues of impredicativity).
"""
  , category: Style
  , examples:
      { includeModuleHeader: false
      , passingCode:
          [ "type R = { a :: Int }"
          , "type R = { a :: Int, b' :: Maybe Int }"
          , "type R = { alpha :: Int, beta' :: Maybe Int }"
          , "type PersonLikeOptics person = { _firstName :: ALens' person String, _lastName :: ALens' person String, _ageInYears :: APrism' person Int }"
          , "newtype R a = R { alpha :: a, beta' :: Maybe a }"
          , "data Fruit = Apple { alpha' :: Maybe Int } | Banana"
          , "data Fruit = Apple | Banana { alpha' :: Maybe Int }"
          ]
      , failingCode:
          [ "type R = { a :: Int, b :: Maybe Int }"
          , "type R a = { a :: a, b :: Maybe a }"
          , "type R a = { alpha :: a, beta :: Maybe a }"
          , "type PersonLikeOptics person = { firstName :: ALens' person String, _lastName :: ALens' person String, _ageInYears :: APrism' person Int }"
          , "type PersonLikeOptics person = { _firstName :: ALens' person String, _lastName :: ALens' person String, ageInYears :: APrism' person Int }"
          , "newtype R a = R { a :: a, b :: Maybe a }"
          , "newtype R a = R { alpha :: a, beta :: Maybe a, charlie :: Maybe String }"
          , "data Fruit = Apple { a :: Maybe Int } | Banana"
          , "data Fruit = Apple | Banana { alpha :: Maybe Int }"
          ]
      }
  , configJsonSchema: containerToPatternMapJsonSchema
  , defaultConfig:
      Object.fromFoldable
        [ Tuple "Maybe" $ exampleRegex "(.*)'"
        , Tuple "ALens" $ exampleRegex "_(.*)"
        , Tuple "ALens'" $ exampleRegex "_(.*)"
        , Tuple "APrism" $ exampleRegex "_(.*)"
        , Tuple "APrism'" $ exampleRegex "_(.*)"
        , Tuple "AnAffineTraversal" $ exampleRegex "_(.*)"
        , Tuple "AnAffineTraversal'" $ exampleRegex "_(.*)"
        ]
  , moduleIssueIdentifier: \containerToPatternObject _systemConfig ->
      let
        containerToPatternMap = containerToPatternObject # Object.toUnfoldable # (Map.fromFoldable :: Array _ -> _)
      in
        typeIssueIdentifier $ case _ of
          TypeRow wrappedRow -> inWrappedRow containerToPatternMap wrappedRow
          TypeRecord wrappedRow -> inWrappedRow containerToPatternMap wrappedRow
          _ -> []
  }
  where
  inWrappedRow containerToPatternMap = case _ of
    (Wrapped { value: Row { labels: Just labels } }) ->
      Separated.values labels >>= forField
    _ -> []

    where
    forField (Labeled { label: Name { token: { range }, name: Label fieldName }, value }) =
      case value of
        TypeApp (TypeConstructor (QualifiedName { name: Proper container })) _ ->
          Map.lookup container containerToPatternMap
            # foldMap \(RegexJson regex) -> guard (not Regex.test regex fieldName)
                [ { message: "Field with container type `" <> container <> "' does not match the naming convention `" <> show regex <> "`", sourceRange: range } ]
        _ -> []

containerToPatternMapJsonSchema :: Object Json
containerToPatternMapJsonSchema = Object.fromHomogeneous
  { "type": encodeString "object"
  , "patternProperties": encodeJson
      { "(.*)":
          { "type": "string"
          , description: "A regular expression that the field name should match."
          }
      }
  }
