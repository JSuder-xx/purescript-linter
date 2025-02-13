module Linter.ModuleRules.Formatting.IfThenElse where

import Prelude

import Data.Monoid (guard)
import Linter.ModuleRule (RuleCategory(..), expressionIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Types (Expr(..))

ifThenElseLeftAligned :: ModuleRule.ModuleRule
ifThenElseLeftAligned = ModuleRule.mkWithNoConfig
  { name: "IfThenElseLeftAligned"
  , category: Formatting
  , description: "Aligning if/then/else tokens consistently helps readability. Left aligning is simply one aesthetic choice."
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ """
x =
  if 1 == 2
    then 3
    else 4
            """
          , """
x =
  if 1 == 2 then 3
    else 4
          """
          , """
x =
  if 1 == 2
       then 3
    else 4
          """
          , """
x =
    if 1 == 2
  then 3
    else 4
          """
          , """
x = if 1 == 2
  then 3
  else 4
          """
          ]
      , passingCode:
          [ "x = if 1 == 2 then 3 else 4"
          , """
x =
  if 1 == 2 then 3
  else 4
                """
          , """
x =
  if 1 == 2
  then 3
  else 4
                """
          , """
x =
  if 1 == 2
  then 3
  else if 1 == 2
  then 4
  else 5
                """
          , """
x =
  if 1 == 2
  then 3
  else if 1 == 2
  then 4
  else if 1 == 2
  then 5
  else 6
                """
          , """
x =
  if 1 == 2 then 3
  else if 1 == 2
  then 4
  else if 1 == 2 then 5
  else 6
                """
          , """
x =
  if 1 == 2 then do
    blee
    blah
    bloo
  else if 1 == 2 then do
    dingo
    bingo
    flamingo
  else if 1 == 2 then do
    sherbert
    flerbert
  else do
    bippity
    boppity
    boop
                """

          ]
      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
      ExprIf { keyword: { range: { start: if' } }, then: { range: thenRange@{ start: then' } }, else: { range: elseRange@{ start: else' } } } ->
        let
          -- NOTE: This is a really horrific hack to handle the case when the if follows an else on the same line.
          -- Here is why
          -- - It would be **easy** to recurse through if/then/else patterns while the else expression is another if/then/else, accumulating an array of if/then/else
          --   and use that complete information to properly report.
          -- - HOWEVER, that would only work for the top/first. The CST traversal machinery would then match each one of the child if/then/else's in turn and those would not
          --   have the entire picture and so would fail.
          -- - To solve this problem would require either finer control over the visitation or, easier, would be some state/context that could be threaded through. This could hold
          --   a set of already visited positions or something that would inform subsequent or child if/then/else expressions to skip/pass/ignore. If a few rules end up requiring this
          --   context then I'll think about it, but for now this crappy-but-simple code should catch 99% of the bad code.
          isIfAligned x = if'.column == x.column || if'.column - 5 == x.column
        in
          ( guard (not $ (if'.line == then'.line) || (isIfAligned then'))
              $ pure { message: "When `if` and `then` are on separate lines they must be left aligned.", sourceRange: thenRange }
          )
            <>
              ( guard (not $ (then'.line == else'.line) || (isIfAligned else'))
                  $ pure { message: "When `then` and `else` are on separate lines then `else` must left align with `if`.", sourceRange: elseRange }
              )
      _ -> []
  }
