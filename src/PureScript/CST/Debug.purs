module PureScript.CST.Debug where

import Prelude

import Data.Array as Array
import Data.Foldable (fold, intercalate)
import Data.Tuple (Tuple(..))

type Label a =
  { name :: String
  , description :: String
  , childKinds :: Array (Tuple String (Array a))
  }

indent :: String -> String
indent s = s <> "  "

debugStr :: forall a. (a -> Label a) -> String -> a -> String
debugStr label indentation = label >>> \{ name, description, childKinds } ->
  indentation <> name <> ": " <> description <> if Array.null childKinds then "" else (fold $ childKind <$> childKinds)
  where
  childKind :: Tuple String (Array a) -> String
  childKind (Tuple label' children) =
    if Array.null children then ""
    else fold
      [ "\n"
      , indent indentation
      , label'
      , "\n"
      , intercalate "\n" $ debugStr label (indent $ indent indentation) <$> children
      ]
