module Linter.ModuleRules.Style.ModuleExports (exportsRequired, requireDocumentation) where

import Prelude

import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String (Pattern(..))
import Linter.ModuleRule (Issue, RuleCategory(..), moduleIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import Options.Applicative.Internal.Utils as String
import PureScript.CST.Separated as Separated
import PureScript.CST.Types (Comment(..), Declaration(..), DelimitedNonEmpty, Export(..), Foreign(..), Ident(..), Labeled(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), Operator(..), Proper(..), SourceToken, Wrapped(..))
import PureScript.CST.Types as CST

exportsRequired :: ModuleRule.ModuleRule
exportsRequired = ModuleRule.mkWithNoConfig
  { name: "ModuleExports.Required"
  , category: Style
  , description:
      "Requiring explicit exports ensures that developers are thinking about encapsulation and avoids missing opportunities to minimize the public surface area."
  , examples:
      { includeModuleHeader: true
      , failingCode:
          [ """
module FlimFlam where

x :: Int
x = 1
          """
          ]
      , passingCode:
          [ """
module FlimFlam (x) where

x :: Int
x = 1
          """
          ]
      }
  , moduleIssueIdentifier: const $ moduleIssueIdentifier $ case _ of
      CST.Module { header: ModuleHeader { name: Name { token: { range: sourceRange } }, exports: Nothing } } ->
        [ { message: "Module should have explicit exports.", sourceRange } ]
      _ -> []
  }

requireDocumentation :: ModuleRule.ModuleRule
requireDocumentation = ModuleRule.mkWithNoConfig
  { name: "ModuleExports.RequireDocumentation"
  , category: Style
  , description:
      "Require documentation of anything exported."
  , examples:
      { includeModuleHeader: true
      , failingCode:
          [ """
module FlimFlam where

x :: Int
x = 1
          """
          , """
module FlimFlam where

-- This is not code documentation.
x :: Int
x = 1
          """
          , """
module FlimFlam where

data Fruit = Apple | Banana | Cherry
          """
          , """
module FlimFlam where

data Fruit :: forall k. k -> Type
data Fruit a = Apple | Banana | Cherry
          """
          , """
module FlimFlam where

class CanFoo a where
  foo :: a -> String
          """
          , """
module FlimFlam (class CanFoo) where

class CanFoo a where
  foo :: a -> String
          """
          , """
module FlimFlam (class Foo) where

-- | Documented the type but not the class
data Foo = A | B

class Foo a
          """
          , """
module FlimFlam (Foo) where

data Foo = A | B

-- | Documented the class but not the type.
class Foo a
          """

          ]
      , passingCode:
          [ """
module FlimFlam where

-- | This is an x.
x :: Int
x = 1
          """
          , """
module FlimFlam where

-- | Fruit is tasty.
data Fruit = Apple | Banana | Cherry
          """
          , """
module FlimFlam (y) where

x :: Int
x = 1

-- | Documented
y :: Int
y = 2
          """
          , """
module FlimFlam where

data Fruit :: forall k. k -> Type
-- | Fruit is tasty.
data Fruit a = Apple | Banana | Cherry
          """
          , """
module FlimFlam where

-- | Indicates something can foo.
class CanFoo a where
  foo :: a -> String
          """
          , """
module FlimFlam (x) where

-- | Hi!
x :: Int
x = 1

class CanFoo a where
  foo :: a -> String
          """
          , """
module FlimFlam (Foo) where

-- | Hi!
data Foo = A | B

class Foo a
          """
          , """
module FlimFlam (class Foo) where

data Foo = A | B

-- | Hi!
class Foo a
          """
          ]
      }
  , moduleIssueIdentifier: const $ moduleIssueIdentifier \(CST.Module { body: ModuleBody { decls }, header: ModuleHeader { exports } }) ->
      decls >>= checkDeclaration (exports # maybe (const true) exportsToSet) -- [ { message: "Module should have explicit exports.", sourceRange } ]
  }
  where
  exportsToSet :: forall e. DelimitedNonEmpty (Export e) -> (String -> Boolean)
  exportsToSet (Wrapped { value }) = flip Set.member $ Set.fromFoldable $ mapMaybe exportToString $ Separated.values value

  classPrefix s = "class" <> s

  exportToString :: forall e. Export e -> Maybe String
  exportToString = case _ of
    ExportValue (Name { name: Ident name }) -> Just name
    ExportOp (Name { name: Operator name }) -> Just name
    ExportType (Name { name: Proper name }) _ -> Just name
    ExportTypeOp _ (Name { name: Operator name }) -> Just name
    ExportClass _ (Name { name: Proper name }) -> Just $ classPrefix name
    ExportModule _ (Name { name: ModuleName name }) -> Just name
    ExportError _ -> Nothing

  isDocumentationComment = case _ of
    Comment s -> String.startsWith (Pattern "-- |") s
    Space _ -> false
    Line _ _ -> false

  tokenHasDocumentation = Array.any isDocumentationComment <<< _.leadingComments

  checkDeclaration :: forall e. (String -> Boolean) -> Declaration e -> Array Issue
  checkDeclaration isExported = case _ of
    DeclData dataHead _ctors' -> checkDataHead dataHead
    DeclType dataHead _ _type -> checkDataHead dataHead
    DeclNewtype dataHead _ _name _type -> checkDataHead dataHead
    DeclClass { keyword, name: Name { name: Proper name } } _namedTypes' -> check keyword $ classPrefix name
    DeclSignature (Labeled { label: Name { token, name: Ident name } }) -> check token name
    DeclForeign _token1 _token2 (ForeignData _sourceToken (Labeled { label: Name { token, name: Proper name } })) -> check token name
    DeclForeign _token1 _token2 (ForeignValue (Labeled { label: Name { token, name: Ident name } })) -> check token name
    DeclForeign _token1 _token2 (ForeignKind _sourceToken _name) ->
      -- ATTENTION: Requiring that types and classes put their documentation on the type/class itself and not on a kind signature.
      -- This seems consistent with the language server today but this could conceivably change.
      []
    DeclKindSignature _token _nameType ->
      -- ATTENTION: Requiring that types and classes put their documentation on the type/class itself and not on a kind signature.
      -- This seems consistent with the language server today but this could conceivably change.
      []
    DeclInstanceChain _ -> []
    DeclDerive _ _ _ -> []
    DeclValue _ ->
      -- The compiler is going to complain if any exported values lack a signature so not bothering to check values.
      []
    DeclFixity _ -> []
    DeclRole _ _ _ _ -> []
    DeclError _ -> []
    where
    check token name =
      if isExported name && not tokenHasDocumentation token then [ { message: "Exported '" <> name <> "' should be documented.", sourceRange: token.range } ]
      else []

    checkDataHead :: forall r. { keyword :: SourceToken, name :: Name Proper | r } -> Array Issue
    checkDataHead { keyword, name: Name { name: Proper name } } = check keyword name
