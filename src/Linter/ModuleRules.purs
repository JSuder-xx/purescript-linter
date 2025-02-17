module Linter.ModuleRules where

import Linter.ModuleRule (ModuleRule)
import Linter.ModuleRules.Formatting.AlignedParenthesis as AlignedParenthesis
import Linter.ModuleRules.Formatting.ApplicationIndentation as ApplicationIndentation
import Linter.ModuleRules.Formatting.ArrayFormatting as ArrayFormatting
import Linter.ModuleRules.Formatting.IfThenElse as IfThenElse
import Linter.ModuleRules.Formatting.LetBinding as LetBinding
import Linter.ModuleRules.Formatting.RecordFormatting as RecordFormatting
import Linter.ModuleRules.Formatting.WhereClause as WhereClause
import Linter.ModuleRules.Style.ModuleExports as ModuleExports
import Linter.ModuleRules.Style.ModuleImports as ModuleImports
import Linter.ModuleRules.Style.MonoidSimplifications as MonoidSimplifications
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
  , ArrayFormatting.rule
  , IfThenElse.ifThenElseLeftAligned
  , LetBinding.compact
  , ModuleExports.exportsRequired
  , ModuleImports.qualification
  , MonoidSimplifications.replaceMaybeMemptyWithFoldMap
  , MonoidSimplifications.useFoldForRepeatedMappends
  , MonoidSimplifications.useGuardOverIfThenElseMEmpty
  , MonoidSimplifications.useGuardOverIfThenMemptyElse
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
  [ ModuleExports.exportsRequired
  , ModuleImports.qualification
  , MonoidSimplifications.replaceMaybeMemptyWithFoldMap
  , MonoidSimplifications.useFoldForRepeatedMappends
  , MonoidSimplifications.useGuardOverIfThenElseMEmpty
  , NoDuplicateTypeclassConstraints.rule
  , UnnecessaryDo.rule
  , UnnecessaryParenthesis.rule
  , UseAnonymous.forOperations
  , UseAnonymous.forRecordUpdates
  , UseAnonymous.forRecordCreation
  , UseForallSymbol.rule
  , UsePunning.rule
  ]
