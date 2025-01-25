module PureScript.CST.QualifiedName where

import Prelude

import Data.Newtype (unwrap)
import PureScript.CST.Types (QualifiedName)

name :: forall a. QualifiedName a -> a
name = unwrap >>> _.name
