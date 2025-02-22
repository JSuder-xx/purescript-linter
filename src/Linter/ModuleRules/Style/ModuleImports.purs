module Linter.ModuleRules.Style.ModuleImports where

import Prelude

import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.Maybe (maybe')
import Data.Monoid (guard)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Extra (RegexJson(..), exampleRegex)
import Data.Tuple (Tuple(..))
import Linter.ModuleRule (ModuleRule, RuleCategory(..), mkModuleRule, moduleIssueIdentifier)
import PureScript.CST.Import as Import
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Separated as Separated
import PureScript.CST.Types (ImportDecl(..), ModuleName(..), Name(..), Wrapped(..))
import PureScript.CST.Types as CST

qualification :: ModuleRule
qualification = mkModuleRule
  { name: "ModuleImportQualification"
  , description:
      """Use this rule to ensure that developers
- Are not importing functions, values, and types with ambiguous names ex. `fromFoldable` is available for many container types such that a lack of qualification is confusing.
- Are qualifying modules consistently.
  """
  , category: Style
  , examples:
      { includeModuleHeader: true
      , passingCode:
          includeModuleName <$>
            [ "import Data.Array (blurble)" -- single import not on the list
            , "import Data.Array (foo, bar, baz)" -- multiple imports not on the list
            , "import Data.Array (fromFoldable) as Array" -- function on the list but module qualified with the correct name
            , "import Data.List.Lazy (fromFoldable) as List.Lazy" -- qualified correct qualification name
            , "import Data.Whatever (fromFoldable)" -- import on the list but module not a match
            , "import Data.Whatever" -- module name not on list so open ended is allowed
            ]
      , failingCode:
          includeModuleName <$>
            [ "import Data.Array (fromFoldable)" -- single module with a single failing import
            , "import Data.Array (foo, bar, fromFoldable)" -- single module with two passing imports
            , "import Data.Array (foo, fromFoldable, bar, toUnfoldable)" -- single module with two passing and two failing imports
            , "import Data.Array" -- failed because open ended module and on the list of modules to match
            , "import Data.Array (fromFoldable) as Arr" -- qualified but with the wrong qualification name
            , "import Data.List.Lazy (fromFoldable) as List" -- qualified but with the wrong qualification name
            ]
      }
  , defaultConfig:
      [ { module: exampleRegex $ fold
            [ "^Data."
            , "("
            , String.joinWith "|"
                [ "Array"
                , "Array.NonEmpty"
                , "Either"
                , "Lens"
                , "List"
                , "List.Lazy"
                , "Map"
                , "Maybe"
                , "Set"
                ]
            , ")$"
            ]
        , qualifyAs: "$1"
        , import: exampleRegex $ String.joinWith "|"
            [ "fromFoldable"
            , "toUnfoldable"
            , "fromString"
            , "singleton"
            , "member"
            , "over"
            , "set"
            , "get"
            ]
        }
      ] :: Array ImportMatchRuleJson
  , moduleIssueIdentifier: \importMatchJsons _systemConfig ->
      let
        importMatches = fromJson <$> importMatchJsons
      in
        moduleIssueIdentifier $ \(CST.Module { header: CST.ModuleHeader { imports } }) ->
          verifyImportDeclaration importMatches =<< imports
  }
  where
  includeModuleName s = "module Test where\n" <> s

  verifyImportDeclaration importMatches (importDecl@(ImportDecl { module: Name { name: ModuleName moduleName }, names, qualified })) =
    foldMap applyRule firstMatchingRule'
    where
    firstMatchingRule' = Array.find (_.module >>> flip Regex.test moduleName) importMatches

    applyRule importMatchRule = qualified #
      maybe'
        -- if not qualified then ensure developer is not importing anything ambiguously... either open or specifically problematic
        assertNoAmbiguousImports
        -- if qualified then check that the qualification uses the right name
        assertCorrectQualification
      where
      assertCorrectQualification (Tuple _sourceToken (qualificationName@(Name { name: ModuleName actualQualifiedModuleName }))) =
        let
          expectedQualifiedModuleName = Regex.replace importMatchRule.module importMatchRule.qualifyAs moduleName
        in
          guard (actualQualifiedModuleName /= expectedQualifiedModuleName)
            [ { message: "Expecting module " <> moduleName <> " to be qualified as " <> expectedQualifiedModuleName, sourceRange: rangeOf qualificationName } ]
      assertNoAmbiguousImports _ = names # maybe'
        (\_ -> [ { message: "Cannot import open ended when requiring qualification.", sourceRange: rangeOf importDecl } ])
        (\(Tuple _sourceToken (Wrapped { value: separatedImports })) -> separatedImports # Separated.values >>= verifyImport importMatchRule.import)

  verifyImport moduleMatch import' = import' # Import.name' # foldMap \importName ->
    guard (Regex.test moduleMatch importName) [ { message: "Import of '" <> importName <> "' must be qualified.", sourceRange: rangeOf import' } ]

type ImportMatchRuleJson =
  { module :: RegexJson
  , import :: RegexJson
  , qualifyAs :: String
  }

type ImportMatch =
  { module :: Regex
  , import :: Regex
  , qualifyAs :: String
  }

fromJson :: ImportMatchRuleJson -> ImportMatch
fromJson { qualifyAs, module: RegexJson moduleRegex, import: RegexJson importRegex } =
  { qualifyAs, module: moduleRegex, import: importRegex }
