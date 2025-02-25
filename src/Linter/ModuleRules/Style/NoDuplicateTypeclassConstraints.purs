module Linter.ModuleRules.Style.NoDuplicateTypeclassConstraints (rule) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.Map.Extra (indexedBy)
import Data.Monoid (guard)
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..))
import Linter.ModuleRule (RuleCategory(..), typeIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Fold (OnPureScript)
import PureScript.CST.Traversal (foldMapType)
import PureScript.CST.Type (debugType)
import PureScript.CST.Types (Proper(..), QualifiedName(..))
import PureScript.CST.Types as CST

rule :: ModuleRule.ModuleRule
rule = ModuleRule.mkWithNoConfig
  { name: "NoDuplicateTypeclassConstraints"
  , category: Style
  , description: "The compiler does not complain about repeated type class constraints on a function, but it is unnecessary noise. This can happen during source control merges."
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ "f :: forall a. Ord a => Ord a => a -> a -> Boolean"
          , "f :: forall a. Ord a => Ord b => Ord a => a -> a -> Boolean"
          , "f :: forall a b. Coercible a b => Coercible a b => a -> Boolean"
          , "f :: forall a. Ord a => Ord a => Blarg a -> Blarg a"
          , "f :: forall a b. Bif a b => Bif a b => Blarg a b -> Blarg a b"
          , "f :: forall a b. Bif a b => Ord a => Eq a => Bif a b => Blarg a b -> Blarg a b"
          , "f :: forall a. Bif a Blarg => Bif a Blarg => Int -> Int"
          , "f :: forall a b. Bif a (Blarg b) => Bif a (Blarg b) => Int -> Int"
          , "f :: forall a b. Bif a { | b } => Bif a { | b } => Int -> Int"
          , "f :: String ->  (forall a b. Bif a { | b } => Bif a { | b } => Int -> Int)"
          ]
      , passingCode:
          [ "f :: forall a. Ord a => a -> a -> Boolean"
          , "f :: forall a b. Ord a => Ord b => a -> Boolean"
          , "f :: forall a b c. Coercible a b => Coercible b c => a -> Boolean"
          , "f :: forall a. Blarg a -> Blarg a"
          , "f :: forall a. Ord a => Blarg a -> Blarg a"
          , "f :: forall x y z. Bifunctor x y => Bifunctor y z -> Int -> Int"
          , "f :: forall a b  . Bif a (Blarg b) => Bif (Blarg b) a => Int -> Int"
          , "f :: forall a b c. Bif a (Blarg b) => Bif a (Blarg c) => Int -> Int"
          , "f :: forall a b c. Bif a { | b } => Bif a { | a } => Int -> Int"
          , "f :: String -> (forall a b c. Bif a { | b } => Bif a { | a } => Int -> Int)"
          ]
      }
  , moduleIssueIdentifier: const $ typeIssueIdentifier $ case _ of
      CST.TypeForall _token _variableNames _token' type' ->
        foldMapType constraints type'
          # indexedBy (\{ typeConstructorName, typeDescriptions } -> { typeConstructorName, typeDescriptions })
          # Map.toUnfoldable
          >>= \(Tuple { typeConstructorName } (NonEmpty head rest)) ->
            guard (not $ Array.null rest)
              $ pure
                  { message: "Constraint " <> typeConstructorName <> " has been applied redundantly."
                  , sourceRange: head.typeConstructorRange
                  }
      _ -> []
  }

type ClassConstraints = Array { typeConstructorRange :: CST.SourceRange, typeConstructorName :: String, typeDescriptions :: Array String }

type TypeClassConstraintMapping = OnPureScript ClassConstraints

constraints :: TypeClassConstraintMapping
constraints = (mempty :: TypeClassConstraintMapping)
  { onType = case _ of
      CST.TypeConstrained
        (CST.TypeApp (CST.TypeConstructor (QualifiedName { token: { range: typeConstructorRange }, name: Proper typeConstructorName })) types)
        _tokFatArrow
        _rightType ->
        [ { typeConstructorRange, typeConstructorName, typeDescriptions: NonEmptyArray.toArray types <#> debugType "  " } ]
      _ -> []
  }
