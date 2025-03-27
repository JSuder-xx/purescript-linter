module Linter.ModuleRules.Style.AvoidTypeAliases where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.JsonSchema (JsonSchema)
import Data.JsonSchema as JsonSchema
import Data.Monoid (guard)
import Data.Set as Set
import Data.Tuple (snd)
import Linter.CodeExamples (exampleWithExports)
import Linter.ModuleRule (RuleCategory(..), declarationIssueIdentifierInModule, exportedDeclarationIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Type as Type
import PureScript.CST.Types (Declaration(..), Name(..), Proper(..), QualifiedName(..), Type(..), Wrapped(..))

ofOtherTypesDirectly :: ModuleRule.ModuleRule
ofOtherTypesDirectly = ModuleRule.mkModuleRule
  { name: "AvoidTypeAliases.OfOtherTypesDirectly"
  , category: Style
  , description:
      """This is a very opinionated rule that forbids using a type alias for direct and simple aliasing of other types such as `type X = String` because they provide neither type safety nor code savings.

Instead of a type alias one might
- Author an actual `newtype` wrapping the other type. This provides safety and documentation.
- Place values of this kind into a record as a named field. This provides documentation and greatly reduces the chances a value will be used incorrectly in a context.

Type aliases of this kind are often used as a stepping stone to developing a full type and certainly have a place in the development process. Only apply this rule
- To areas of the codebase that require the highest quality and have the most re-use such as library code, Domain or Entity models, etc..
- When code is completely finished and ready to be merged. It might not be a good idea to fail the normal CI build for a feature branch as this would flag work in-progress. To do this author one config file with active development rules and another with final merge rules and run the CI pipeline accordingly.
"""
  , examples:
      { includeModuleHeader: true
      , failingCode:
          exampleWithExports { exports: [ "A" ] } <$>
            [ "type A = String"
            , "type A a = String"
            , "type A a = Int"
            , "type A a = Blarg a"
            ]
      , passingCode:
          exampleWithExports { exports: [ "A" ] } <$>
            [ "type B = String" -- B is not exported so fine
            , "type A = { a :: Int }"
            , "type A a = { a :: Int }"
            , "type A = ( a :: Int )"
            , "type A r = ( a :: Int | r )"
            , "type A = Maybe String"
            , "type A = Array (Tuple String String)"
            , "type A a = Array (Tuple a String)"
            , "type A a = Flurf Derb a"
            , "type A = Int -> Int -> Int"
            , "type A a = Int -> a -> Int"
            , "type A a = forall b. a -> b -> Int"
            , "type A = Map Int String"
            , "type A = SomeOtherType Int String"
            , "type A = SomeOtherType (Tuple String String)"
            ]
      }
  , configJsonSchema: JsonSchema.object { onlyExported: onlyExportedSchema }
  , defaultConfig: { onlyExported: true }
  , moduleIssueIdentifier: \{ onlyExported } _ -> (if onlyExported then exportedDeclarationIssueIdentifier else declarationIssueIdentifierInModule) case _ of
      DeclType { name: Name { token } } _ type_ -> case type_ of
        TypeConstructor _ ->
          [ { message: "Avoid type aliases that are direct aliases of other types because they provide neither safety nor code savings. Use `newtype` or document by naming these values as fields in a record."
            , sourceRange: token.range
            }
          ]
        TypeApp _ otherTypes -> guard (NonEmptyArray.all Type.isTypeVariable otherTypes)
          [ { message: "Avoid type aliases that are direct aliases of other types because they provide neither safety nor code savings. Use `newtype` or document by naming these values as fields in a record."
            , sourceRange: token.range
            }
          ]
        _ -> []
      _ -> []
  }

onlyExportedSchema :: JsonSchema
onlyExportedSchema = JsonSchema.addDescription "When true this rule applies only to exported data types." JsonSchema.boolean

withAnonymousRecordsInContainerTypes :: ModuleRule.ModuleRule
withAnonymousRecordsInContainerTypes = ModuleRule.mkModuleRule
  { name: "AvoidTypeAliases.WithAnonymousRecordsInContainerTypes"
  , category: Style
  , description:
      """This is a VERY opinionated rule that forbids defining an alias that consists of some container **OF** an anonymous record ex. `type X = Array { name :: String, isCool :: Boolean }`.

Instead one might consider type aliasing the contained type. For example, in `type Points = Array { x :: Number, y :: Number }` the single point record should be aliased `type Point = { x :: Number, y :: Number }.`
"""
  , examples:
      { includeModuleHeader: true
      , failingCode:
          exampleWithExports { exports: [ "A" ] } <$>
            [ "type A = Maybe { thing :: String }"
            , "type A a = Maybe { thing :: String, a :: a }"
            , "type A a = Map String { thing :: String, a :: a }"
            , "type A a = Array (forall b. { thing :: String, a :: a, b :: b })"
            ]
      , passingCode:
          exampleWithExports { exports: [ "A" ] } <$>
            [ "type A = Maybe Widget"
            , "type B = Maybe { thing :: String }" -- type B is not exported
            , "type A a = Maybe (Widget a)"
            , "type A a = Map String (Widget a)"
            , "type A = Maybe (Person -> Boolean)"
            ]
      }
  , configJsonSchema: JsonSchema.object
      { containers: JsonSchema.addDescription "List of container types that will be checked for anonymous records."
          $ JsonSchema.arrayOf
          $ JsonSchema.addDescription "Name of a container type such as `Maybe` or `Array`." JsonSchema.string
      , onlyExported: onlyExportedSchema
      }
  , defaultConfig:
      { onlyExported: true
      , containers:
          [ "Maybe"
          , "Set"
          , "Array"
          , "Map"
          , "NonEmptyArray"
          , "Parser"
          , "ParserT"
          , "State"
          , "StateT"
          ]
      }
  , moduleIssueIdentifier: \{ containers, onlyExported } _ ->
      let
        isContainerType = flip Set.member $ Set.fromFoldable containers
      in
        (if onlyExported then exportedDeclarationIssueIdentifier else declarationIssueIdentifierInModule) case _ of
          DeclType { name: Name { token } } _ type_ -> case type_ of
            TypeApp (TypeConstructor (QualifiedName { name: Proper name })) typesNES ->
              guard (isContainerType name && NonEmptyArray.any declaresRecord typesNES) [ { message: "Avoid type aliases that starting with well-known single argument 'container' types because it obfuscates the rich Api of the container. You may want to alias the CONTAINED type instead.", sourceRange: token.range } ]
            _ -> []
          _ -> []

  }
  where
  declaresRecord = case _ of
    TypeRecord _ -> true
    TypeRow _ -> true

    TypeParens (Wrapped { value }) -> declaresRecord value
    TypeForall _ _varBindings _ theType -> declaresRecord theType
    TypeConstrained x _ y -> declaresRecord x || declaresRecord y
    TypeApp theType otherTypes -> declaresRecord theType || NonEmptyArray.any declaresRecord otherTypes
    TypeOp theType operatorTypeNES -> declaresRecord theType || NonEmptyArray.any (declaresRecord <<< snd) operatorTypeNES

    TypeArrow _ _ _ -> false
    TypeVar _name -> false
    TypeConstructor _qName -> false
    TypeWildcard _ -> false
    TypeHole _name -> false
    TypeString _ _ -> false
    TypeInt _ _ _ -> false
    TypeKinded _ _ _ -> false
    TypeOpName _ -> false
    TypeArrowName _ -> false
    TypeError _ -> false
