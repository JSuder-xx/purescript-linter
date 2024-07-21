module PureScript.CST.Expr where

import Prelude

import Data.Array as Array

import Data.Map.Extra (keyCountLookup)
import Data.Maybe (Maybe(..))
import PureScript.CST.Fold (OnPureScript)
import PureScript.CST.QualifiedName as QualifiedName
import PureScript.CST.Types (Expr(..), Ident, QualifiedName)

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

exprIdent :: forall e13. Expr e13 -> Maybe (QualifiedName Ident)
exprIdent = case _ of
  ExprIdent x -> Just x
  _ -> Nothing

fetchExpressions :: OnPureScript (Array (Expr Void))
fetchExpressions = (mempty :: OnPureScript (Array (Expr Void))) { onExpr = pure }

mkIdentifierCount
  :: forall target
   . (OnPureScript (Array (Expr Void)) -> target -> Array (Expr Void))
  -> target
  -> (Ident -> Int)
mkIdentifierCount f x = keyCountLookup $ QualifiedName.name <$> (Array.mapMaybe exprIdent $ f fetchExpressions x)