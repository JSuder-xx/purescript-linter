module Linter.ModuleRules where

import Linter.ModuleRule (ModuleRule)
import Linter.ModuleRules.AlignedParenthesis as AlignedParenthesis
import Linter.ModuleRules.Application as Application
import Linter.ModuleRules.ArrayFormatting as ArrayFormatting
import Linter.ModuleRules.IfThenElse as IfThenElse
import Linter.ModuleRules.LetBinding as LetBinding
import Linter.ModuleRules.ModuleExports as ModuleExports
import Linter.ModuleRules.MonoidSimplifications as MonoidSimplifications
import Linter.ModuleRules.NoDuplicateTypeclassConstraints as NoDuplicateTypeclassConstraints
import Linter.ModuleRules.RecordFormatting as RecordFormatting
import Linter.ModuleRules.UnnecessarParenthesis as UnnecessarParenthesis
import Linter.ModuleRules.UnnecessaryDo as UnnecessaryDo
import Linter.ModuleRules.UseAnonymous as UseAnonymous
import Linter.ModuleRules.UseForallSymbol as UseForallSymbol
import Linter.ModuleRules.UsePunning as UsePunning
import Linter.ModuleRules.WhereClause as WhereClause

allModuleRules :: Array ModuleRule
allModuleRules =
  [ AlignedParenthesis.rule
  , Application.inArray
  , Application.inRecord
  , ArrayFormatting.rule
  , IfThenElse.ifThenElseLeftAligned
  , LetBinding.compact
  , ModuleExports.exportsRequired
  , MonoidSimplifications.replaceMaybeMemptyWithFoldMap
  , MonoidSimplifications.useFoldForRepeatedMappends
  , MonoidSimplifications.useGuardOverIfThenElseMEmpty
  , MonoidSimplifications.useGuardOverIfThenMemptyElse
  , NoDuplicateTypeclassConstraints.rule
  , RecordFormatting.rule
  , UnnecessaryDo.rule
  , UnnecessarParenthesis.rule
  , UseAnonymous.forOperations
  , UseAnonymous.forRecordUpdates
  , UseAnonymous.forRecordCreation
  , UseForallSymbol.rule
  , UsePunning.rule
  , WhereClause.whereLeftAligned
  ]
