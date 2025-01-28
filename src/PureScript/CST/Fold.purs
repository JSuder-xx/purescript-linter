module PureScript.CST.Fold where

import Prelude

import PureScript.CST.Types as CST

type OnKind cstType monoid = cstType Void -> monoid

type OnPureScript monoid =
  { onDecl :: OnKind CST.Declaration monoid
  , onBinder :: OnKind CST.Binder monoid
  , onExpr :: OnKind CST.Expr monoid
  , onType :: OnKind CST.Type monoid
  }

type OnModule monoid =
  { onModule :: OnKind CST.Module monoid
  , onPureScript :: OnPureScript monoid
  }
