module PureScript.CST.Import where

import Data.Maybe (Maybe(..))
import PureScript.CST.Types (Ident(..), Import(..), Name(..), Proper(..))

name' :: forall e. Import e -> Maybe String
name' = case _ of
  ImportValue (Name { name: Ident n }) -> Just n
  ImportOp _ -> Nothing
  ImportType (Name { name: Proper n }) _ -> Just n
  ImportTypeOp _ _ -> Nothing
  ImportClass _ (Name { name: Proper n }) -> Just n
  ImportError _ -> Nothing
