# Overview

A linter for PureScript with rules that check both formatting and stylistic issues.

## Getting Started

Download and install the linter from NPM

```
npm install purescript-linter
```

Initialize the configuration file

```
npx purescript-linter --init
```

This creates a configuration file which determines which rules apply to specified filepaths.

## Rules Overview

There are two categories of rule

- Formatting rules assert how code should be formatted. This can be useful if you cannot, for company policy reasons, or do not wish to use a code formatter such as the excellent `purs-tidy`.
- Style rules enforce coding style such as requiring code documentation for exports, asking developers to use record field punning when available, etc..

## Rules

### Formatting

Formatting rules cover lexical formatting concerns that might overwise be handled by a formatter/pretty printer.
Use these rules when unable to use a formatter in your codebase. For example, perhaps a formatter does not conform to prescribed rules.

#### AlignedParenthesis

Aligning Parentheses helps the reader visually parse the two tokens.

#### Application.InArray.IndentArguments

This very limited rule ensures proper function call argument indentation when the function call is made inside an array.

While limited, this rule helps with the formatting of function calls in render functions.

#### Application.InRecord.IndentArguments

This rule has a very limited scope: It ensures proper function call argument indentation when the function call is made while declaring a record literal.

#### ArrayFormatting

Ensures consistent spacing when declaring an array literal.

#### IfThenElse.LeftAligned

Aligning if/then/else tokens consistently helps readability. Left aligning is simply one aesthetic choice.

#### LetBinding.VerticalCompact

This let formatting rule prioritizes conservation of vertical space.

#### RecordFormatting

Ensures consistent spacing when declaring a record literal.

#### WhereClause.LeftAligned

Consistent formatting of the where clause helps readability. Left aligning the where keyword with the bindings is simply one choice.

### Style

Style rules cover code "correctness" or stylistic choices. For example, there is a rule to prefer the use of punning when available.

#### Applicative.UnlessAndWhen

This rule asks that developers replace

- `when (not EXPR)` with `unless EXPR`
- `unless (not EXPR)` with `when EXPR`

In order to remove negation. Human beings read positive statements more easily.

#### AvoidSameComparison

This rule forbids comparing two equivalent expressions because this will always return either true or false.

- `a == a`
- `a /= a`
- `a > a`
- `a >= a`
- etc..

#### ModuleExports.RequireDocumentation

Require code documentation `-- |` of anything exported by a module to help developers understand usage.

#### ModuleExports.Required

Requiring explicit exports ensures that developers are thinking about encapsulation and avoids missing opportunities to minimize the public surface area.

#### ModuleImports.Forbid

Use this rule to ensure modules do not import other modules. This can be used as a cheap way to enforce decoupling between namespaces.

#### ModuleImports.RequireQualification

Use this rule to ensure that developers

- Are not importing functions, values, and types with ambiguous names ex. `fromFoldable` is available for many container types such that a lack of qualification is confusing.
- Are qualifying modules consistently.

#### Monoid.ReplaceIfThenElseMEmptyWithGuard

Replacing `if EXPR then TRUE else mempty` with `guard EXPR TRUE` reduces cognitive overhead because the reader should not be \"interested\" in the false branch.

This rule ONLY applies when the TRUE branch is considered a quickly executing expression. Expressions are considered quickly executing if they do not make function calls.
The reason expressions must be quickly executing is that the TRUE branch always evaluates when using `guard` and if that is slow you are better off using `if/then/else` which
delays execution until necessary.

This is similar to using `when EXPR TRUE` rather than `if EXPR then TRUE else pure unit` when working the applicatives.

#### Monoid.ReplaceIfthenMEmptyElseWithGuard

Replacing `if EXPR then mempty else FALSE` with `guard (not EXPR) FALSE` _may_ reduce cognitive overhead. However, this case is a little more controversial.

This rule ONLY applies when the FALSE branch is considered a "quickly executing expression". Expressions are considered quickly executing if they do not make function calls.
The reason expressions must be quickly executing is that the expression always evaluates when using `guard` and if that is slow you are better off using `if/then/else` which
delays execution until necessary.

#### Monoid.ReplaceMaybeMemptyWithFoldMap

Replacing `maybe mempty` with `foldMap` is a bit more succinct and more clearly expresses the intention.

#### Monoid.ReplaceRepeatedMappendsWithFold

Using `fold`

1. Removes the need for parenthesis which reduces lexical noise.
2. Is more succinct, in terms of characters, when there are 8 or more mappends.

#### NoDuplicateTypeclassConstraints

The compiler does not complain about repeated type class constraints on a function, but it is unnecessary noise. This can happen during source control merges.

#### NoUnnecessaryDo

A Monadic bind followed by a pure is actually a Functor map. It is more truthful to represent this as a narrower Functor map.

A `do` with no binds and no let declarations is unnecessary. Readers should expect a sequence of monadic binds when they see the `do` keyword.

#### NoUnnecessaryParenthesis

Using parenthesis when unnecessary harms readability with unnecessary noise.

This rule identifies

- Unnecessary parenthesis in type declarations.
- SOME of the cases of unnecessary parenthesis in expressions. It is nowhere near as robust as hlint.

#### UseAnonymous.BinaryOperations

It can be easier to read a wildcard operation than a lambda when the lambda body consists of a single binary operation.

With the default configuration this rule only applies to relational operations such as `<`, `<=`, `>`, `>=`, and `<>`.

#### UseAnonymous.RecordCreation

It can be easier to read a wildcard record creation than to visually tie the arguments to the fields where they are used.

For example `\firstName lastName -> { firstName, middleInitial: "", lastName, suffix: "" }` could be re-written `{ firstName: _, middleInitial: "", lastName: _, suffix: "" }`.

#### UseAnonymous.RecordUpdates

It is easier to read a wildcard record update than a lambda.

#### UseForallSymbol

Some projects prefer using the ΓêÇ symbol rather than 'forall' to improve type signature readability and require less typing.

#### UsePunning

Punning is easier to read because it reduces the noise of unnecessary repetition. By removing unnecessary repetition, true differences stand out more.

For example, can you spot the difference in `{ alice: alice, bob: bob, cindy: cindy', dave: dave }`?
Now with punning that is `{ alice, bob, cindy: cindy', dave }`.
