module PureScript.CST.Fold where

import Prelude

import PureScript.CST.Types as CST

type OnKind f m = f Void -> m

type OnPureScript m =
  { onDecl :: OnKind CST.Declaration m
  , onBinder :: OnKind CST.Binder m
  , onExpr :: OnKind CST.Expr m
  , onType :: OnKind CST.Type m
  }

type OnModule m =
  { onModule :: OnKind CST.Module m
  , onPureScript :: OnPureScript m
  }
