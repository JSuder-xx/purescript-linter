module PureScript.CST.Expr where

import Prelude

import Data.Array (intercalate, mapMaybe)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..), fst, snd)
import PureScript.CST.Debug (Label, debugStr)
import PureScript.CST.QualifiedName as QualifiedName
import PureScript.CST.Separated as Separated
import PureScript.CST.Types (AppSpine(..), DoStatement(..), Expr(..), Guarded(..), GuardedExpr(..), Ident(..), IntValue(..), LetBinding(..), Name(..), Operator(..), PatternGuard(..), Proper(..), QualifiedName(..), RecordLabeled(..), RecordUpdate(..), Where(..), Wrapped(..))

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

exprParens :: forall e79. Expr e79 -> Maybe (Wrapped (Expr e79))
exprParens = case _ of
  ExprParens x -> Just x
  _ -> Nothing

-- | Return all of the identifiers found at and under this expression.
-- |
-- | 1. Attempted to use the traversal functions but this yielded surprising and inconsistent results.
-- | 2. There counts implicit identifiers from record puns.
allUnqualifiedIdentifiers :: Expr Void -> Array Ident
allUnqualifiedIdentifiers = case _ of
  ExprIdent (QualifiedName { module: Nothing, name }) -> [ name ]
  ExprRecord (Wrapped { value: Just exprs }) ->
    Separated.values exprs
      >>=
        case _ of
          RecordField _ _ expr -> allUnqualifiedIdentifiers expr
          RecordPun (Name { name }) -> [ name ]

  expr -> label expr # _.childKinds >>= snd >>= allUnqualifiedIdentifiers

allParenthesis :: Expr Void -> Array (Wrapped (Expr Void))
allParenthesis = case _ of
  ExprParens x -> [ x ]
  expr -> label expr # _.childKinds >>= snd >>= allParenthesis

appTerm ∷ AppSpine Expr Void → Maybe (Expr Void)
appTerm = case _ of
  AppType _ _ -> Nothing
  AppTerm expr -> Just expr

guardedExprs :: Guarded Void -> Array (Expr Void)
guardedExprs = case _ of
  Unconditional _ where' -> whereExprs where'
  Guarded exprs -> guardedExprExprs =<< NonEmptyArray.toArray exprs

doStatementExprs :: DoStatement Void -> Array (Expr Void)
doStatementExprs = case _ of
  DoLet _ letBinding -> letBindingExprs =<< NonEmptyArray.toArray letBinding
  DoDiscard e -> [ e ]
  DoBind _ _ e -> [ e ]
  DoError _ -> []

guardedExprExprs :: GuardedExpr Void -> Array (Expr Void)
guardedExprExprs (GuardedExpr { patterns, where: where' }) = whereExprs where' <> (patternGuardExprs =<< Separated.values patterns)

whereExprs :: Where Void -> Array (Expr Void)
whereExprs (Where { expr: expr', bindings }) = [ expr' ] <> (letBindingExprs =<< foldMap (NonEmptyArray.toArray <<< snd) bindings)

patternGuardExprs :: PatternGuard Void -> Array (Expr Void)
patternGuardExprs (PatternGuard { expr: expr' }) = [ expr' ]

recordUpdateExprs :: RecordUpdate Void -> Array (Expr Void)
recordUpdateExprs = case _ of
  RecordUpdateLeaf _ _ expr' -> [ expr' ]
  RecordUpdateBranch _ (Wrapped { value }) ->
    Separated.values value >>= recordUpdateExprs

letBindingExprs :: LetBinding Void -> Array (Expr Void)
letBindingExprs =
  case _ of
    LetBindingSignature _ -> []
    LetBindingName { guarded } -> guardedExprs guarded
    LetBindingPattern _ _ where' -> whereExprs where'
    LetBindingError _ -> []

type ExprLabel e = Label (Expr e)

label :: Expr Void -> ExprLabel Void
label = case _ of
  ExprHole (Name { name: Ident name }) -> leaf "ExprHole" name
  ExprSection _ -> leaf "ExprSection" ""
  ExprIdent (QualifiedName { name: Ident name }) -> leaf "ExprIdent" name
  ExprConstructor (QualifiedName { name: Proper name }) -> leaf "ExprConstructor" name
  ExprBoolean _ b -> leaf "ExprBoolean" $ show b
  ExprChar _ c -> leaf "ExprChar" $ show c
  ExprString _ s -> leaf "ExprString" s
  ExprInt _ iv -> leaf "ExprInt" $ case iv of
    SmallInt i -> show i
    BigInt s -> s
    BigHex s -> s
  ExprNumber _ n -> leaf "ExprNumber" $ show n
  ExprArray (Wrapped { value: Nothing }) -> withChildren "ExprArray" []
  ExprArray (Wrapped { value: Just exprs }) -> withChildren "ExprArray" $ Separated.values exprs
  ExprRecord (Wrapped { value: Nothing }) -> withChildren "ExprRecord" []
  ExprRecord (Wrapped { value: Just exprs }) -> withChildren "ExprRecord"
    $ Array.mapMaybe
        case _ of
          RecordField _ _ expr -> Just expr
          _ -> Nothing
    $ Separated.values exprs
  ExprParens (Wrapped { value: expr }) -> singleChildExpr "ExprParens" expr
  ExprTyped expr _ _ -> singleChildExpr "ExprTyped" expr
  ExprError _ -> leaf "ExprError" ""
  ExprInfix expr exprs ->
    { name: "ExprInfix"
    , description: ""
    , childKinds:
        [ Tuple "MainExpr" [ expr ]
        , Tuple "Other" (NonEmptyArray.toArray exprs >>= \(Tuple (Wrapped { value: first }) second) -> [ first, second ])
        ]
    }
  ExprOp expr ops ->
    let
      opsArray = NonEmptyArray.toArray ops
    in
      { name: "ExprOp"
      , description: intercalate ", " $ (un Operator <<< QualifiedName.name <<< fst) <$> opsArray
      , childKinds:
          [ Tuple "MainExpr" [ expr ]
          , Tuple "Ops" $ snd <$> opsArray
          ]
      }
  ExprOpName (QualifiedName { name: Operator operator }) -> leaf "ExprOpName" operator
  ExprNegate _ expr -> singleChildExpr "ExprNegate" expr
  ExprRecordAccessor { expr: expr } -> singleChildExpr "ExprRecordAccessor" expr
  ExprRecordUpdate expr (Wrapped { value: updates }) ->
    { name: "ExprRecordUpdate"
    , description: ""
    , childKinds:
        [ Tuple "MainExpr" [ expr ]
        , Tuple "Updates" $ recordUpdateExprs =<< Separated.values updates
        ]
    }
  ExprApp expr appSpines ->
    { name: "ExprApp"
    , description: ""
    , childKinds:
        [ Tuple "MainExpr" [ expr ]
        , Tuple "AppSpines" (mapMaybe appTerm $ NonEmptyArray.toArray appSpines)
        ]
    }
  ExprLambda { body } -> singleChildExpr "ExprLambda" body
  ExprIf { cond, true: true', false: false' } ->
    { name: "ExprIf"
    , description: ""
    , childKinds:
        [ Tuple "Cond" [ cond ]
        , Tuple "True" [ true' ]
        , Tuple "False" [ false' ]
        ]
    }
  ExprCase ({ head, branches }) ->
    { name: "ExprCase"
    , description: ""
    , childKinds:
        [ Tuple "Head" $ Separated.values head
        , Tuple "Branches" $ guardedExprs =<< snd <$> NonEmptyArray.toArray branches
        ]
    }
  ExprLet { bindings, body } ->
    { name: "ExprLet"
    , description: ""
    , childKinds:
        [ Tuple "Body" [ body ]
        , Tuple "Bindings" $ letBindingExprs =<< NonEmptyArray.toArray bindings
        ]
    }
  ExprDo { statements } ->
    { name: "ExprDo"
    , description: ""
    , childKinds:
        [ Tuple "Statements" $ doStatementExprs =<< NonEmptyArray.toArray statements
        ]
    }
  ExprAdo { result, statements } ->
    { name: "ExprAdo"
    , description: ""
    , childKinds:
        [ Tuple "Result" [ result ]
        , Tuple "Statements" $ doStatementExprs =<< statements
        ]
    }
  where
  leaf :: forall e. String -> String -> ExprLabel e
  leaf name description = { name, description, childKinds: [] }

  withChildren :: forall e. String -> Array (Expr e) -> ExprLabel e
  withChildren name children = { name, description: "", childKinds: [ Tuple "Children" children ] }

  singleChildExpr :: forall e. String -> Expr e -> ExprLabel e
  singleChildExpr name single = { name, description: "", childKinds: [ Tuple "Single" [ single ] ] }

debugExpr :: String -> Expr Void -> String
debugExpr = debugStr label
