module PureScript.CST.Type where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap, intercalate)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..), snd)
import PureScript.CST.Debug (Label, debugStr)
import PureScript.CST.Separated as Separated
import PureScript.CST.Types (Ident(..), IntValue(..), Labeled(..), Name(..), Operator(..), Prefixed(..), Proper(..), QualifiedName(..), Row(..), Type(..), TypeVarBinding(..), Wrapped(..))
import PureScript.CST.Types as Types

infixl 10 Tuple as **

typeVariableName :: forall e. TypeVarBinding (Prefixed (Name Ident)) e -> String
typeVariableName (TypeVarName (Prefixed { value: Name { name: Ident name } })) = name
typeVariableName (TypeVarKinded (Wrapped { value: (Labeled { label: Prefixed { value: Name { name: Ident name } } }) })) = name

type TypeLabel e = Label (Types.Type e)

typeVariable' :: forall e. Types.Type e -> Maybe (Name Ident)
typeVariable' = case _ of
  TypeVar a -> Just a
  _ -> Nothing

isTypeVariable :: forall e. Types.Type e -> Boolean
isTypeVariable = typeVariable' >>> isJust

label :: forall e. Types.Type e -> TypeLabel e
label = case _ of
  TypeVar (Name { name: Ident name }) -> simple "TypeVar" name
  TypeConstructor (QualifiedName { module: module', name: Proper name }) -> simple "TypeConstructor" $ (modulePrefix module') <> name
  TypeWildcard _ -> simple "TypeWildcard" "_"
  TypeHole (Name { name: Ident name }) -> simple "TypeHole" name
  TypeString _ s -> simple "TypeString" s
  TypeInt _ _ iv -> simple "TypeInt" $ case iv of
    SmallInt i -> show i
    BigHex s -> s
    BigInt s -> s
  TypeRow (Wrapped { value: row }) ->
    { name: "TypeRow"
    , description: rowDescription row
    , childKinds: rowTypes row
    }
  TypeRecord (Wrapped { value: row }) ->
    { name: "TypeRecord"
    , description: rowDescription row
    , childKinds: rowTypes row
    }
  TypeForall _ typeVarBindingsNE _ tipe ->
    { name: "TypeForall"
    , description:
        typeVarBindingsNE
          # NonEmptyArray.toArray
          <#> typeVarBindingPrefix
          # String.joinWith ", "
    , childKinds: [ labeledChild "type" tipe ]
    }
  TypeKinded tipe1 _ tipe2 -> withChildren "TypeKinded"
    [ labeledChild "left" tipe1, labeledChild "right" tipe2 ]
  TypeApp tipe typesNE -> withChildren "TypeApp"
    [ labeledChild "type" tipe, "types" ** (NonEmptyArray.toArray typesNE) ]
  TypeOp tipe operatorTypesNE -> withChildren "TypeOp"
    [ labeledChild "type" tipe, "operatorTypes" ** (snd <$> NonEmptyArray.toArray operatorTypesNE) ]
  TypeOpName (QualifiedName { module: module', name: Operator operator }) -> simple "TypeOpName" $ (modulePrefix module') <> operator
  TypeArrow leftTipe _ rightTipe -> withChildren "TypeArrow"
    [ labeledChild "left" leftTipe, labeledChild "right" rightTipe ]
  TypeArrowName _ -> simple "TypeArrowName" ""
  TypeConstrained tipe1 _ tipe2 -> withChildren "TypeConstrained"
    [ labeledChild "left" tipe1, labeledChild "right" tipe2 ]
  TypeParens (Wrapped { value: tipe }) -> withChildren "TypeParens" [ labeledChild "tipe" tipe ]
  TypeError _ -> simple "TypeError" ""
  where
  modulePrefix = foldMap ((_ <> ".") <<< unwrap)
  rowDescription (Row { labels }) = labels # foldMap (Separated.values >>> map (\(Labeled { label: Types.Name { name: label' } }) -> unwrap label') >>> intercalate ", ")
  rowTypes (Row { labels, tail }) = [ Tuple "row types" (labels # foldMap (Separated.values >>> map (\(Labeled { value }) -> value))) ]
    <> (tail # foldMap \(Tuple _ tailType) -> [ labeledChild "tail" tailType ])
  withChildren name childKinds = { name, description: "", childKinds }
  labeledChild name child = name ** [ child ]
  simple name description = { name, description, childKinds: [] }

debugType :: forall e. String -> Types.Type e -> String
debugType = debugStr label

typeVarBindingPrefix :: forall e. TypeVarBinding (Prefixed (Name Ident)) e -> String
typeVarBindingPrefix =
  (\(Name { name: Ident ident }) -> ident) <<<
    case _ of
      TypeVarKinded (Wrapped { value: Labeled { label: Prefixed { value } } }) -> value
      TypeVarName (Prefixed { value }) -> value
