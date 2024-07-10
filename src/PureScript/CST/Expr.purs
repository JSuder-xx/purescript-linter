module PureScript.CST.Expr where

import PureScript.CST.Types (Expr(..))

isTerminal ∷ ∀ e. Expr e → Boolean
isTerminal = case _ of
  ExprHole _ -> true
  ExprSection _ -> true
  ExprIdent _ -> true
  ExprConstructor _ -> true
  ExprBoolean _ _ -> true
  ExprChar _ _ -> true
  ExprString _ _ -> true
  ExprInt _ _ -> true
  ExprNumber _ _ -> true
  _ -> false