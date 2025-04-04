module Linter.ModuleRules.Style.ModuleImports where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.JsonSchema as JsonSchema
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe')
import Data.Monoid (guard)
import Data.Newtype (un)
import Data.NonEmpty (NonEmpty, fromNonEmpty)
import Data.NonEmpty as NonEmpty
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Extra (RegexJson(..), exampleRegex)
import Data.Tuple (Tuple(..))
import Linter.ModuleRule (Issue, ModuleRule, RuleCategory(..), mkModuleRule, mkWithNoConfig, moduleIssueIdentifier)
import PureScript.CST.Import as Import
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Separated as Separated
import PureScript.CST.Types (Export(..), ImportDecl(..), ModuleName(..), Name(..), Separated, Wrapped(..))
import PureScript.CST.Types as CST

qualification :: ModuleRule
qualification = mkModuleRule
  { name: "ModuleImports.RequireQualification"
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
  , configJsonSchema: JsonSchema.arrayOf
      $ JsonSchema.object
          { module: JsonSchema.addDescription "A Regular Expression used to match on the module." JsonSchema.string
          , import: JsonSchema.addDescription "A Regular Expression that matches on things imported." JsonSchema.string
          , qualifyAs: JsonSchema.addDescription "Determines the expected qualification for any matched modules. NOTE: You can use RegEx replacement variables for anything captured by the `module` RegEx." JsonSchema.string
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

forbid :: ModuleRule
forbid = mkModuleRule
  { name: "ModuleImports.Forbid"
  , description:
      """Use this rule to ensure modules do not import other modules. This can be used as a cheap way to enforce decoupling between namespaces."""
  , category: Style
  , examples:
      { includeModuleHeader: true
      , passingCode:
          includeModuleName <$>
            [ "import Data.Good"
            , "import Data.Good (blurble)"
            , "import Data.Good as Good"
            , "import Data.Good (blurble) as G"
            ]
      , failingCode:
          includeModuleName <$>
            [ "import Unsafe.Coerce"
            , "import Unsafe.Coerce.Refined"
            , "import Unsafe.Coerce (blurble)"
            , "import Unsafe.Coerce as Bad"
            , "import Unsafe.Coerce (blurble) as B"
            ]
      }
  , configJsonSchema: JsonSchema.arrayOf $ JsonSchema.addDescription "A regular expression defining the pattern for a forbidden module import." JsonSchema.string
  , defaultConfig: [ exampleRegex "^Unsafe.Coerce(.*)$" ] :: Array RegexJson
  , moduleIssueIdentifier: \importRegexJsons _systemConfig ->
      let
        importRegexs = importRegexJsons <#> un RegexJson
      in
        moduleIssueIdentifier $ \(CST.Module { header: CST.ModuleHeader { imports } }) ->
          verifyImportDeclaration importRegexs =<< imports
  }
  where
  includeModuleName s = "module Test where\n" <> s

  verifyImportDeclaration importRegexs (ImportDecl { keyword: { range }, module: Name { name: ModuleName moduleName } }) =
    foldMap applyRule firstMatchingRule'
    where
    firstMatchingRule' = Array.find (flip Regex.test moduleName) importRegexs
    applyRule _ = [ { message: "Import is forbidden.", sourceRange: range } ]

--------------------------
----------------------
-----------------------
avoidMultipleAliasesOfSameModule :: ModuleRule
avoidMultipleAliasesOfSameModule = mkWithNoConfig
  { name: "ModuleImports.AvoidMultipleAliasesOfSameModule"
  , description: """Use this rule to ensure that a module is not imported under multiple aliases. The only exception is that one duplicate alias is allowed if it is used as a module re-export."""
  , category: Style
  , examples:
      { includeModuleHeader: true
      , passingCode:

          [ """
module Test where

import Data.Array
import Data.Array as Array
import Data.Map as Map
"""
          , """
module Test (module A) where

-- duplicate alias but one of them is a module export and so disregarded
import Data.Array
import Data.Array as Array
import Data.Array as A
import Data.Map as Map
"""
          , """
module Test (module A) where

-- duplicate alias but one of them is a module export and so disregarded
import Data.Array
import Data.Array as A
import Data.Array as Array
import Data.Map as Map
"""
          ]
      , failingCode:

          [ """
module Test where

import Data.Array
import Data.Array as Array
import Data.Map as Map
import Data.Array as A
"""
          ]
      }
  , moduleIssueIdentifier: \_systemConfig ->
      moduleIssueIdentifier $ \(CST.Module { header: CST.ModuleHeader { exports, imports } }) ->
        imports
          # toModuleNameAliasesMap { exportAliases: foldMap reExportAliases exports }
          # Map.toUnfoldable
          # (map @Array (map @(Tuple _) (fromNonEmpty Array.cons >>> Array.sortWith (\(Name { token }) -> token.range.start.line) >>> Array.drop 1)))
          >>= reportDuplicateAliases
  }
  where
  reExportAliases :: Wrapped (Separated (Export Void)) -> Set ModuleName
  reExportAliases = un Wrapped >>> _.value >>> Separated.values >>> map @Array exportModuleName' >>> Array.catMaybes >>> Set.fromFoldable
    where
    exportModuleName' :: Export Void -> Maybe ModuleName
    exportModuleName' = case _ of
      ExportModule _ (Name { name: moduleName }) -> Just moduleName
      _ -> Nothing

  toModuleNameAliasesMap :: { exportAliases :: Set ModuleName } -> Array (ImportDecl Void) -> Map ModuleName (NonEmpty Array (Name ModuleName))
  toModuleNameAliasesMap { exportAliases } = bindFlipped importDeclarationToAlias >>> Map.fromFoldableWith (<>)
    where
    importDeclarationToAlias :: ImportDecl Void -> Array (Tuple ModuleName (NonEmpty Array (Name ModuleName)))
    importDeclarationToAlias (ImportDecl { module: (Name { name: moduleName }), qualified }) =
      qualified
        # foldMap \(Tuple _token alias@(Name { name: aliasModuleName })) -> if Set.member aliasModuleName exportAliases then [] else [ Tuple moduleName $ NonEmpty.singleton alias ]

  reportDuplicateAliases :: Tuple ModuleName (Array (Name ModuleName)) -> Array Issue
  reportDuplicateAliases (Tuple (ModuleName moduleName) aliases) = aliases <#> reportAlias
    where
    reportAlias :: Name ModuleName -> Issue
    reportAlias (Name { name: ModuleName alias, token: { range } }) = { message: "Module '" <> moduleName <> "' has a redundant alias '" <> alias <> "'.", sourceRange: range }
