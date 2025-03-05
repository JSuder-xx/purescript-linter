module PureScript.CST.Equivalent
  ( class Equivalent
  , equivalent
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import PureScript.CST.Types (AppSpine(..), Binder(..), Expr(..), Guarded(..), GuardedExpr(..), Ident, Labeled(..), LetBinding(..), ModuleName, Name(..), Operator, PatternGuard(..), Prefixed(..), Proper, QualifiedName(..), RecordLabeled(..), RecordUpdate(..), Row(..), Separated(..), Type(..), TypeVarBinding(..), Where(..), Wrapped(..))
import PureScript.CST.Types as CST

-- | Anemically realized type class that can test whether two things are equal if we remove source location information.
-- | Since true equivalency is a harder problem than I want to solve this errs conservatively towards false negatives.
-- | - If two things are considered equivalent they ARE equivalent.
-- | - However, two things may be reported as not equivalent which are.
-- |
-- | All of the instances have been written out LOOOOONG hand. If the cst-parser library had given these types instances of Generic then this
-- | could have all been derived by walking the tree and comparing everything for equality EXCLUDING tokens.
class Equivalent a where
  -- | Anemically realized type class that can test whether two things are equal if we remove source location information.
  -- | Since true equivalency is a harder problem than I want to solve this errs conservatively towards false negatives.
  -- | - If two things are considered equivalent they ARE equivalent.
  -- | - However, two things may be reported as not equivalent which are.
  -- |
  -- | All of the instances have been written out LOOOOONG hand. If the cst-parser library had given these types instances of Generic then this
  -- | could have all been derived by walking the tree and comparing everything for equality EXCLUDING tokens.
  equivalent :: a -> a -> Boolean

instance Equivalent a => Equivalent (Array a) where
  equivalent leftA rightA =
    Array.length leftA == Array.length rightA
      && (not $ Array.any (not <<< uncurry equivalent) $ Array.zip leftA rightA)

instance Equivalent a => Equivalent (NonEmptyArray a) where
  equivalent leftA rightA =
    NonEmptyArray.length leftA == NonEmptyArray.length rightA
      && (not $ NonEmptyArray.any (not <<< uncurry equivalent) $ NonEmptyArray.zip leftA rightA)

instance Equivalent Void where
  equivalent _ _ = true

instance (Equivalent a, Equivalent b) => Equivalent (Tuple a b) where
  equivalent (Tuple la lb) (Tuple ra rb) = equivalent la ra && equivalent lb rb

instance Equivalent a => Equivalent (Maybe a) where
  equivalent (Just left) (Just right) = equivalent left right
  equivalent Nothing Nothing = true
  equivalent Nothing _ = false
  equivalent _ Nothing = false

instance Equivalent Ident where
  equivalent = eq

instance Equivalent Proper where
  equivalent = eq

instance Equivalent ModuleName where
  equivalent = eq

instance Equivalent Operator where
  equivalent = eq

instance Equivalent CST.Label where
  equivalent = eq

instance Equivalent e => Equivalent (CST.Prefixed e) where
  equivalent (Prefixed { value: left }) (Prefixed { value: right }) = equivalent left right

instance Equivalent a => Equivalent (Name a) where
  equivalent (Name { name: left }) (Name { name: right }) = equivalent left right

instance Equivalent a => Equivalent (QualifiedName a) where
  equivalent (QualifiedName { module: leftModule, name: leftName }) (QualifiedName { module: rightModule, name: rightName }) = equivalent leftModule rightModule && equivalent leftName rightName

instance Equivalent a => Equivalent (Wrapped a) where
  equivalent (Wrapped { value: left }) (Wrapped { value: right }) = equivalent left right

instance Equivalent a => Equivalent (Separated a) where
  equivalent (Separated { head: leftHead, tail: leftTail }) (Separated { head: rightHead, tail: rightTail }) = equivalent leftHead rightHead && equivalent (snd <$> leftTail) (snd <$> rightTail)

instance (Equivalent a, Equivalent b) => Equivalent (Labeled a b) where
  equivalent (Labeled { label: leftLabel, value: leftValue }) (Labeled { label: rightLabel, value: rightValue }) = equivalent leftLabel rightLabel && equivalent leftValue rightValue

instance Equivalent a => Equivalent (RecordLabeled a) where
  equivalent (RecordPun left) (RecordPun right) = equivalent left right
  equivalent (RecordField leftName _ leftValue) (RecordField rightName _ rightValue) = equivalent leftName rightName && equivalent leftValue rightValue
  equivalent _ _ = false

instance Equivalent e => Equivalent (PatternGuard e) where
  equivalent (PatternGuard { binder: leftBinder, expr: leftExpr }) (PatternGuard { binder: rightBinder, expr: rightExpr }) = (on equivalent (map @Maybe fst) leftBinder rightBinder) && equivalent leftExpr rightExpr

instance Equivalent e => Equivalent (GuardedExpr e) where
  equivalent (GuardedExpr { patterns: leftPatterns, where: leftWhere }) (GuardedExpr { patterns: rightPatterns, where: rightWhere }) = equivalent leftPatterns rightPatterns && equivalent leftWhere rightWhere

instance Equivalent a => Equivalent (Guarded a) where
  equivalent (Unconditional _ leftWhere) (Unconditional _ rightWhere) = equivalent leftWhere rightWhere
  equivalent (Guarded left) (Guarded right) = equivalent left right
  equivalent _ _ = false

instance Equivalent e => Equivalent (Where e) where
  equivalent (Where { expr: leftExpr, bindings: leftBindings }) (Where { expr: rightExpr, bindings: rightBindings }) = equivalent leftExpr rightExpr && on equivalent (map @Maybe snd) leftBindings rightBindings

instance Equivalent e => Equivalent (CST.Row e) where
  equivalent (Row { labels: leftLabels, tail: leftTail }) (Row { labels: rightLabels, tail: rightTail }) = equivalent leftLabels rightLabels && on equivalent (map @Maybe snd) leftTail rightTail

instance Equivalent e => Equivalent (Binder e) where
  equivalent (BinderWildcard _) (BinderWildcard _) = true
  equivalent (BinderVar left) (BinderVar right) = equivalent left right
  equivalent (BinderNamed leftName _ leftBinder) (BinderNamed rightName _ rightBinder) = equivalent leftName rightName && equivalent leftBinder rightBinder
  equivalent (BinderConstructor leftName leftBinders) (BinderConstructor rightName rightBinders) = equivalent leftName rightName && equivalent leftBinders rightBinders
  equivalent (BinderBoolean _ left) (BinderBoolean _ right) = left == right
  equivalent (BinderChar _ left) (BinderChar _ right) = left == right
  equivalent (BinderString _ left) (BinderString _ right) = left == right
  equivalent (BinderInt _ _ left) (BinderInt _ _ right) = left == right
  equivalent (BinderNumber _ _ left) (BinderNumber _ _ right) = left == right
  equivalent (BinderArray left) (BinderArray right) = equivalent left right
  equivalent (BinderRecord left) (BinderRecord right) = equivalent left right
  equivalent (BinderParens left) (BinderParens right) = equivalent left right
  equivalent (BinderTyped leftBinder _ leftType) (BinderTyped rightBinder _ rightType) = equivalent leftBinder rightBinder && equivalent leftType rightType
  equivalent (BinderOp leftBinder leftOps) (BinderOp rightBinder rightOps) = equivalent leftBinder rightBinder && equivalent leftOps rightOps
  equivalent (BinderError left) (BinderError right) = equivalent left right
  equivalent _ _ = false

instance (Equivalent a, Equivalent e) => Equivalent (TypeVarBinding a e) where
  equivalent (TypeVarKinded left) (TypeVarKinded right) = equivalent left right
  equivalent (TypeVarName left) (TypeVarName right) = equivalent left right
  equivalent _ _ = false

instance Equivalent e => Equivalent (CST.Type e) where
  equivalent (TypeVar left) (TypeVar right) = equivalent left right
  equivalent (TypeConstructor left) (TypeConstructor right) = equivalent left right
  equivalent (TypeWildcard _) (TypeWildcard _) = true
  equivalent (TypeHole left) (TypeHole right) = equivalent left right
  equivalent (TypeString _ left) (TypeString _ right) = left == right
  equivalent (TypeInt _ _ left) (TypeInt _ _ right) = left == right
  equivalent (TypeRow left) (TypeRow right) = equivalent left right
  equivalent (TypeRecord left) (TypeRecord right) = equivalent left right
  equivalent (TypeForall _ leftTypeVars _ leftType) (TypeForall _ rightTypeVars _ rightType) = equivalent leftType rightType && equivalent leftTypeVars rightTypeVars
  equivalent (TypeKinded leftA _ leftB) (TypeKinded rightA _ rightB) = equivalent leftA rightA && equivalent leftB rightB
  equivalent (TypeApp leftType leftTypeArgs) (TypeApp rightType rightTypeArgs) = equivalent leftType rightType && equivalent leftTypeArgs rightTypeArgs
  equivalent (TypeOp leftType leftTypeOperands) (TypeOp rightType rightTypeOperands) = equivalent leftType rightType && equivalent leftTypeOperands rightTypeOperands
  equivalent (TypeOpName left) (TypeOpName right) = equivalent left right
  equivalent (TypeArrow leftA _ leftB) (TypeArrow rightA _ rightB) = equivalent leftA rightA && equivalent leftB rightB
  equivalent (TypeArrowName _) (TypeArrowName _) = true
  equivalent (TypeConstrained leftA _ leftB) (TypeConstrained rightA _ rightB) = equivalent leftA rightA && equivalent leftB rightB
  equivalent (TypeParens left) (TypeParens right) = equivalent left right
  equivalent (TypeError left) (TypeError right) = equivalent left right
  equivalent _ _ = false

instance Equivalent e => Equivalent (LetBinding e) where
  equivalent (LetBindingSignature left) (LetBindingSignature right) = equivalent left right
  equivalent (LetBindingName left) (LetBindingName right) =
    equivalent left.name right.name && equivalent left.binders right.binders && equivalent left.guarded right.guarded
  equivalent (LetBindingPattern leftBinder _ leftWhere) (LetBindingPattern rightBinder _ rightWhere) = equivalent leftBinder rightBinder && equivalent leftWhere rightWhere
  equivalent (LetBindingError left) (LetBindingError right) = equivalent left right
  equivalent _ _ = false

instance Equivalent e => Equivalent (RecordUpdate e) where
  equivalent (RecordUpdateLeaf leftName _ leftExpr) (RecordUpdateLeaf rightName _ rightExpr) = equivalent leftName rightName && equivalent leftExpr rightExpr
  equivalent (RecordUpdateBranch leftName leftUpdates) (RecordUpdateBranch rightName rightUpdates) = equivalent leftName rightName && equivalent leftUpdates rightUpdates
  equivalent _ _ = false

instance (Equivalent (f e), Equivalent e) => Equivalent (AppSpine f e) where
  equivalent (AppType _ left) (AppType _ right) = equivalent left right
  equivalent (AppTerm left) (AppTerm right) = equivalent left right
  equivalent _ _ = false

instance Equivalent e => Equivalent (Expr e) where
  equivalent (ExprHole left) (ExprHole right) = equivalent left right
  equivalent (ExprSection _) (ExprSection _) = true
  equivalent (ExprIdent left) (ExprIdent right) = equivalent left right
  equivalent (ExprConstructor left) (ExprConstructor right) = equivalent left right
  equivalent (ExprBoolean _ left) (ExprBoolean _ right) = left == right
  equivalent (ExprChar _ left) (ExprChar _ right) = left == right
  equivalent (ExprString _ left) (ExprString _ right) = left == right
  equivalent (ExprInt _ left) (ExprInt _ right) = left == right
  equivalent (ExprNumber _ left) (ExprNumber _ right) = left == right
  equivalent (ExprArray left) (ExprArray right) = equivalent left right
  equivalent (ExprError left) (ExprError right) = equivalent left right
  equivalent (ExprParens left) (ExprParens right) = equivalent left right
  equivalent (ExprNegate _ left) (ExprNegate _ right) = equivalent left right
  equivalent (ExprOp leftOp leftOpParams) (ExprOp rightOp rightOpParams) = equivalent leftOp rightOp && equivalent leftOpParams rightOpParams
  equivalent (ExprTyped left _ leftType) (ExprTyped right _ rightType) = equivalent left right && equivalent leftType rightType
  equivalent (ExprRecord left) (ExprRecord right) = equivalent left right
  equivalent (ExprInfix left leftOperands) (ExprInfix right rightOperands) = equivalent left right && equivalent leftOperands rightOperands
  equivalent (ExprOpName left) (ExprOpName right) = equivalent left right
  equivalent (ExprRecordAccessor { expr: leftExpr, path: leftPath }) (ExprRecordAccessor { expr: rightExpr, path: rightPath }) = equivalent leftExpr rightExpr && equivalent leftPath rightPath
  equivalent (ExprRecordUpdate leftExpr leftUpdates) (ExprRecordUpdate rightExpr rightUpdates) = equivalent leftExpr rightExpr && equivalent leftUpdates rightUpdates
  equivalent (ExprApp leftFn leftSpines) (ExprApp rightFn rightSpines) = equivalent leftFn rightFn && equivalent leftSpines rightSpines
  equivalent (ExprIf { cond: lCond, true: lTrue, false: lFalse }) (ExprIf { cond: rCond, true: rTrue, false: rFalse }) =
    equivalent lCond rCond && equivalent lTrue rTrue && equivalent lFalse rFalse

  -- Not even going to bother with more complex expressions and especially expressions that introduce names because
  -- true equivalency requires variable re-writing ex. `\x -> x + 1`` is equivalent to `\y -> y + 1`.
  equivalent (ExprLet _) (ExprLet _) = false
  equivalent (ExprLambda _) (ExprLambda _) = false
  equivalent (ExprDo _) (ExprDo _) = false
  equivalent (ExprAdo _) (ExprAdo _) = false
  equivalent (ExprCase _) (ExprCase _) = false

  equivalent _ _ = false
