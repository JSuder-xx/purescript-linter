module PureScript.CST.ModuleHeader (isDeclarationExported) where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import PureScript.CST.Separated as Separated
import PureScript.CST.Types (Declaration(..), DelimitedNonEmpty, Export(..), Foreign(..), Ident(..), Labeled(..), ModuleHeader(..), ModuleName(..), Name(..), Operator(..), Proper(..), Wrapped(..))
import PureScript.CST.Types as CST

data ExportName
  = NameOfTypeOrValue String
  | NameOfClass String

derive instance Eq ExportName
derive instance Ord ExportName

isDeclarationExported :: forall e. ModuleHeader e -> (CST.Declaration e -> Boolean)
isDeclarationExported (ModuleHeader { exports }) = exports
  # maybe
      (const true) -- if no explicit exports then everything exported
      isExportedFromExports
  where
  isExportedFromExports :: DelimitedNonEmpty (Export e) -> (CST.Declaration e -> Boolean)
  isExportedFromExports (Wrapped { value }) = case _ of
    DeclData dataHead _ctors' -> isDataHeadExported dataHead
    DeclType dataHead _ _type -> isDataHeadExported dataHead
    DeclNewtype dataHead _ _name _type -> isDataHeadExported dataHead
    DeclClass { name: Name { name: Proper name } } _namedTypes' -> isExported $ NameOfClass name
    DeclSignature (Labeled { label: Name { name: Ident name } }) -> isExported $ NameOfTypeOrValue name
    DeclForeign _token1 _token2 (ForeignData _sourceToken (Labeled { label: Name { name: Proper name } })) -> isExported $ NameOfTypeOrValue name
    DeclForeign _token1 _token2 (ForeignValue (Labeled { label: Name { name: Ident name } })) -> isExported $ NameOfTypeOrValue name
    DeclValue { name: Name { name: Ident name } } -> isExported $ NameOfTypeOrValue name
    DeclForeign _token1 _token2 (ForeignKind _sourceToken _name) -> false
    DeclKindSignature _token _nameType -> false
    DeclInstanceChain _ -> false
    DeclDerive _ _ _ -> false
    DeclFixity _ -> false
    DeclRole _ _ _ _ -> false
    DeclError _ -> false

    where
    isExported :: ExportName -> Boolean
    isExported = flip Set.member $ Set.fromFoldable $ mapMaybe toExportName' $ Separated.values value

    isDataHeadExported :: forall r. { name :: Name Proper | r } -> Boolean
    isDataHeadExported { name: Name { name: Proper name } } = isExported $ NameOfTypeOrValue name

  toExportName' :: Export e -> Maybe ExportName
  toExportName' = case _ of
    ExportValue (Name { name: Ident name }) -> Just $ NameOfTypeOrValue name
    ExportOp (Name { name: Operator name }) -> Just $ NameOfTypeOrValue name
    ExportType (Name { name: Proper name }) _ -> Just $ NameOfTypeOrValue name
    ExportTypeOp _ (Name { name: Operator name }) -> Just $ NameOfTypeOrValue name
    ExportClass _ (Name { name: Proper name }) -> Just $ NameOfClass name
    ExportModule _ (Name { name: ModuleName name }) -> Just $ NameOfTypeOrValue name
    ExportError _ -> Nothing
