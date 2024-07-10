module PureScript.CST.Type where

import PureScript.CST.Types (Ident(..), Labeled(..), Name(..), Prefixed(..), TypeVarBinding(..), Wrapped(..))

typeVariableName :: forall e. TypeVarBinding (Prefixed (Name Ident)) e -> String
typeVariableName (TypeVarName (Prefixed { value: Name { name: Ident name } })) = name
typeVariableName (TypeVarKinded (Wrapped { value: (Labeled { label: Prefixed { value: Name { name: Ident name } } }) })) = name