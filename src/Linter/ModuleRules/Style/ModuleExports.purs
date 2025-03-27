module Linter.ModuleRules.Style.ModuleExports (exportsRequired, requireDocumentation) where

import Prelude

import Data.Array as Array
import Data.JsonSchema as JsonSchema
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.String (Pattern(..))
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Extra (RegexJson(..), exampleRegex)
import Linter.ModuleRule (Issue, RuleCategory(..), exportedDeclarationIssueIdentifier, moduleIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import Options.Applicative.Internal.Utils as String
import PureScript.CST.Types (Comment(..), Declaration(..), Foreign(..), Ident(..), Labeled(..), ModuleHeader(..), Name(..), Proper(..), SourceToken)
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
requireDocumentation = ModuleRule.mkModuleRule
  { name: "ModuleExports.RequireDocumentation"
  , category: Style
  , description:
      "Require code documentation `-- |` of anything exported by a module to help developers understand usage. This can be configured with a regex pattern of types/type classes/values to exclude from this requirement."
  , configJsonSchema: JsonSchema.maybe
      $ JsonSchema.addDescription "Regex of exported names that do NOT require documentation. When null then all exported types/values require documentation." JsonSchema.string
  , defaultConfig: Just $ exampleRegex "^component$"
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

-- | This is a value.
x :: Int
x = 1
          """
          , """
module FlimFlam where

-- | This is a type.
data Fruit = Apple | Banana | Cherry
          """
          , """
module FlimFlam where

component :: Int
component = 10
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
  , moduleIssueIdentifier: \ignoreRegexJson' _ -> exportedDeclarationIssueIdentifier $ checkDeclaration { ignoreRegex': un RegexJson <$> ignoreRegexJson' }
  }
  where
  isDocumentationComment = case _ of
    Comment s -> String.startsWith (Pattern "-- |") s
    Space _ -> false
    Line _ _ -> false

  tokenHasDocumentation = Array.any isDocumentationComment <<< _.leadingComments

  checkDeclaration :: forall e. { ignoreRegex' :: Maybe Regex } -> Declaration e -> Array Issue
  checkDeclaration { ignoreRegex' } = case _ of
    DeclData dataHead _ctors' -> checkDataHead dataHead
    DeclType dataHead _ _type -> checkDataHead dataHead
    DeclNewtype dataHead _ _name _type -> checkDataHead dataHead
    DeclClass { keyword, name: Name { name: Proper name } } _namedTypes' -> check keyword name
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
    shouldIgnore = ignoreRegex' # maybe (const false) Regex.test
    check token name =
      if not shouldIgnore name && not tokenHasDocumentation token then [ { message: "Exported '" <> name <> "' should be documented.", sourceRange: token.range } ]
      else []

    checkDataHead :: forall r. { keyword :: SourceToken, name :: Name Proper | r } -> Array Issue
    checkDataHead { keyword, name: Name { name: Proper name } } = check keyword name
