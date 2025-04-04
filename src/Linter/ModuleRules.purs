module Linter.ModuleRules where

import Linter.ModuleRule (ModuleRule)
import Linter.ModuleRules.Formatting.AlignedParenthesis as AlignedParenthesis
import Linter.ModuleRules.Formatting.ApplicationIndentation as ApplicationIndentation
import Linter.ModuleRules.Formatting.ArrayFormatting as ArrayFormatting
import Linter.ModuleRules.Formatting.IfThenElse as IfThenElse
import Linter.ModuleRules.Formatting.LetBinding as LetBinding
import Linter.ModuleRules.Formatting.RecordFormatting as RecordFormatting
import Linter.ModuleRules.Formatting.WhereClause as WhereClause
import Linter.ModuleRules.Style.ApplicativeSimplifications as ApplicativeSimplifications
import Linter.ModuleRules.Style.AvoidSameComparison as AvoidSameComparison
import Linter.ModuleRules.Style.AvoidTypeAliases as AvoidTypeAliases
import Linter.ModuleRules.Style.ModuleExports as ModuleExports
import Linter.ModuleRules.Style.ModuleImports as ModuleImports
import Linter.ModuleRules.Style.MonoidSimplifications as MonoidSimplifications
import Linter.ModuleRules.Style.NamingPatterns as NamingPatterns
import Linter.ModuleRules.Style.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Linter.ModuleRules.Style.UnnecessaryDo as UnnecessaryDo
import Linter.ModuleRules.Style.UnnecessaryParenthesis as UnnecessaryParenthesis
import Linter.ModuleRules.Style.UseAnonymous as UseAnonymous
import Linter.ModuleRules.Style.UseForallSymbol as UseForallSymbol
import Linter.ModuleRules.Style.UsePunning as UsePunning

allModuleRules :: Array ModuleRule
allModuleRules =
  [ AlignedParenthesis.rule
  , ApplicationIndentation.inArray
  , ApplicationIndentation.inRecord
  , ApplicativeSimplifications.unlessAndWhen
  , ArrayFormatting.rule
  , AvoidSameComparison.avoidSameComparison
  , AvoidTypeAliases.ofOtherTypesDirectly
  , AvoidTypeAliases.withAnonymousRecordsInContainerTypes
  , IfThenElse.ifThenElseLeftAligned
  , LetBinding.compact
  , ModuleExports.exportsRequired
  , ModuleExports.requireDocumentation
  , ModuleImports.avoidMultipleAliasesOfSameModule
  , ModuleImports.forbid
  , ModuleImports.qualification
  , MonoidSimplifications.replaceMaybeMemptyWithFoldMap
  , MonoidSimplifications.useFoldForRepeatedMappends
  , MonoidSimplifications.useGuardOverIfThenElseMEmpty
  , MonoidSimplifications.useGuardOverIfThenMemptyElse
  , NamingPatterns.fieldsOfContainerTypes
  , NoDuplicateTypeclassConstraints.rule
  , RecordFormatting.rule
  , UnnecessaryDo.rule
  , UnnecessaryParenthesis.rule
  , UseAnonymous.forOperations
  , UseAnonymous.forRecordUpdates
  , UseAnonymous.forRecordCreation
  , UseForallSymbol.rule
  , UsePunning.rule
  , WhereClause.whereLeftAligned
  ]

recommendedRules :: Array ModuleRule
recommendedRules =
  [ ApplicativeSimplifications.unlessAndWhen
  , AvoidSameComparison.avoidSameComparison
  , ModuleExports.exportsRequired
  , ModuleImports.avoidMultipleAliasesOfSameModule
  , ModuleImports.forbid
  , ModuleImports.qualification
  , MonoidSimplifications.replaceMaybeMemptyWithFoldMap
  , MonoidSimplifications.useFoldForRepeatedMappends
  , MonoidSimplifications.useGuardOverIfThenElseMEmpty
  , NoDuplicateTypeclassConstraints.rule
  , UnnecessaryDo.rule
  , UnnecessaryParenthesis.rule
  , UseAnonymous.forRecordUpdates
  , UseAnonymous.forRecordCreation
  , UsePunning.rule
  ]
