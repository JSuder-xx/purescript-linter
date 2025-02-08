# Overview

A very basic linter for PureScript built using the fantastic CST parser written by Nate Faubion.

## Why?

- If you cannot use `purescript-tidy` for team / company policy reasons.
- Includes rules beyond formatting.

## Rules

### Formatting

#### AlignedParenthesis

Aligning Parentheses helps the reader visually parse the two tokens.

#### Application.InArray.IndentArguments

This very limited rule ensures proper function call argument indentation when the function call is made inside an array.

While limited, this rule helps with the formatting of function calls in render functions.

#### Application.InRecord.IndentArguments

This rule has a very limited scope: It ensures proper function call argument indentation when the function call is made while declaring a record literal.

#### ArrayFormatting

Ensures consistent spacing when declaring an array literal.

#### IfThenElseLeftAligned

Aligning if/then/else tokens consistently helps readability. Left aligning is simply one aesthetic choice.

#### LetBinding-VerticalCompact

This let formatting rule prioritizes conservation of vertical space.

#### RecordFormatting

Ensures consistent spacing when declaring a record literal.

#### WhereClauseLeftAligned

Consistent formatting of the where clause helps readability. Left aligning the where keyword with the bindings is simply one choice.

### Style

#### ModuleExportsRequired

Requiring explicit exports ensures that developers are thinking about encapsulation and avoids missing opportunities to minimize the public surface area.

#### NoDuplicateTypeclassConstraints

The compiler does not complain about repeated type class constraints on a function, but it is unnecessary noise. This can happen during source control merges.

#### NoUnnecessaryDo

A Monadic bind followed by a pure is actually a Functor map. It is more truthful to represent this as a narrower Functor map.

A `do` with no binds and no let declarations is unnecessary. Readers should expect a sequence of monadic binds when they see the `do` keyword.

#### NoUnnecessaryParenthesis

Using parenthesis when unnecessary harms readability.

#### ReplaceMaybeMemptyWithFoldMap

Replacing `maybe mempty` with `foldMap` is a bit more succinct and more clearly expresses the intention.

#### UseAnonymous-ForOperations

It is easier to read a wildcard operations than a lambda.

#### UseAnonymous-ForRecordCreation

It is easier to read a wildcard record creation than to visually tie the arguments to the fields where they are used.

#### UseAnonymous-ForRecordUpdates

It is easier to read a wildcard record update than a lambda.

#### UseFoldForRepeatedMappends

Using `fold`

1. Removes the need for parenthesis which reduces lexical noise.
2. Is more succinct, in terms of characters, when there are 8 or more mappends.

#### UseForallSymbol

Some projects prefer using the ∀ symbol rather than 'forall' to improve type signature readability and require less typing.

#### UseGuardOverIfThenElseMEmpty

Replacing `if EXPR then TRUE else mempty` with `guard EXPR TRUE` reduces cognitive overhead because the reader should not be "interested" in the false branch.

#### UseGuardOverIfThenMEmptyElse

Replacing `if EXPR then mempty else FALSE` with `guard (not EXPR) FALSE` _may_ reduce cognitive overhead. However, this case is a little more controversial.

#### UsePunning

Punning is easier to read because it reduces the noise of unnecessary repetition. By removing unnecessary repetition, true differences stand out more.

For example, can you spot the difference in `{ alice: alice, bob: bob', cindy: cindy', dave: dave }`?
Now with punning that is `{ alice, bob, cindy: cindy', dave }`.
