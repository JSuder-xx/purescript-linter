module Test.CST where

import Prelude

import Data.Array as Array
import Data.Maybe (maybe)
import Data.Newtype (un)
import PureScript.CST.Expr as Expr
import PureScript.CST.Types (Declaration(..), Expr, Ident(..), Module(..), ModuleBody(..))
import Test.Common (assertCode, simpleModulePrefix)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

declaredValueExpressions :: Module Void -> Array (Expr Void)
declaredValueExpressions (Module { body: ModuleBody { decls } }) =
  decls
    >>=
      ( case _ of
          DeclValue { guarded } -> Expr.guardedExprs guarded
          _ -> mempty
      )

cst :: Spec Unit
cst =
  describe "PureScript.CST" do
    describe "Expr" do
      describe "allUnqualifiedIdentifiers" do
        let
          expectNames descr code names =
            it descr $ assertCode (simpleModulePrefix <> code)
              $ declaredValueExpressions
                  >>> Array.head
                  >>> maybe
                    (fail "No value expressions found")
                    (Expr.allUnqualifiedIdentifiers >>> Array.sort >>> map (un Ident) >>> shouldEqual names)

        expectNames "Simple Function Call" "x = a b" [ "a", "b" ]

        expectNames "Operations" "x = \\a' -> guard (a' `op` to) $> a'" [ "a'", "a'", "guard", "op", "to" ]

        expectNames "Multiple Expression Terms"
          """
f = x a + y b + z c
          """
          [ "a", "b", "c", "x", "y", "z" ]

        expectNames "Lambda with Function Application"
          """
f = \x -> a b c d
        """
          [ "a", "b", "c", "d" ]

        expectNames "Deeply Nested Let"
          """
f =
  let
    x =
      let
        y =
          let
            z = 1
            q = r 10 + s 10 + (if t then u else v)
          in
            z + 1
      in
        y + 1
  in
    x + 10
                """
          [ "r", "s", "t", "u", "v", "x", "y", "z" ]

        expectNames "Nested Records"
          """
z =
  let
    x =
      { field1: a + 1
      , field2: { nested: { reallyNested1: b, reallyNested2: c + (d 10) + if e { f1: f } then g else h } }
      }
  in
    x + 10
                """
          [ "a", "b", "c", "d", "e", "f", "g", "h", "x" ]

        expectNames "Record with pun"
          """
f =
  { a
  , b
  , c: a + 1
  , d: a (a + 1)
  }
          """
          [ "a", "a", "a", "a", "b" ]
