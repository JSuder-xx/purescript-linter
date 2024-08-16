module Rule.NoDuplicateTypeclassConstraints (rule) where

import Prelude

import Data.Array as Array
import Data.Array as String
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.Map.Extra (indexedBy)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Rule (declarationLintProducer)
import Rule as Rule
import PureScript.CST.Fold (OnPureScript)
import PureScript.CST.Traversal (foldMapType)
import PureScript.CST.Types (Declaration(..), Ident(..), Labeled(..), Name(..), Proper(..), QualifiedName(..))
import PureScript.CST.Types as CST

rule :: Rule.Rule
rule = Rule.mkWithNoConfig
  { name: "NoDuplicateTypeclassConstraints"
  , examples:
      { bad:
          [ "f :: forall a. Ord a => Ord a => a -> a -> Boolean"
          , "f :: forall a. Ord a => Ord b => Ord a => a -> a -> Boolean"
          , "f :: forall a b. Coercible a b => Coercible a b => a -> Boolean"
          , "f :: forall a. Ord a => Ord a => Blarg a -> Blarg a"
          ]
      , good:
          [ "f :: forall a. Ord a => a -> a -> Boolean"
          , "f :: forall a b. Ord a => Ord b => a -> Boolean"
          , "f :: forall a b c. Coercible a b => Coercible b c => a -> Boolean"
          , "f :: forall a. Blarg a -> Blarg a"
          , "f :: forall a. Ord a => Blarg a -> Blarg a"
          ]
      }
  , lintProducer: declarationLintProducer $ case _ of
      DeclSignature (Labeled { value: (CST.TypeForall _token _variableNames _token' type') }) ->
        (foldMapType constraints type')
          # indexedBy (\{ typeConstructorName, typeVariableNames } -> { typeConstructorName, typeVariableNames })
          # Map.toUnfoldable
          >>= \(Tuple { typeConstructorName, typeVariableNames } (NonEmpty head rest)) ->
            guard (not $ Array.null rest)
              $ pure
                  { message: "Constraint " <> typeConstructorName <> " has been applied to the type variables " <> String.intercalate "," typeVariableNames <> " redundantly."
                  , sourceRange: head.typeConstructorRange
                  }
      _ -> []
  }

type ClassConstraints = Array { typeConstructorRange :: CST.SourceRange, typeConstructorName :: String, typeVariableNames :: Array String }

type TypeClassConstraintMapping = OnPureScript ClassConstraints

constraints :: TypeClassConstraintMapping
constraints = (mempty :: TypeClassConstraintMapping)
  { onType = case _ of
      CST.TypeConstrained
        (CST.TypeApp (CST.TypeConstructor (QualifiedName { token: { range }, name: Proper typeConstructorName })) ne)
        _tokFatArrow
        _rightType ->
        NonEmptyArray.toArray ne <#> _typeVar # sequence # maybe [] \typeVariableNames -> [ { typeConstructorRange: range, typeConstructorName, typeVariableNames } ]
      _ -> []
  }

_typeVar :: forall e. CST.Type e -> Maybe String
_typeVar = case _ of
  CST.TypeVar (Name { name: Ident x }) -> Just x
  _ -> Nothing