# Rules

## Formatting (8)

Formatting rules cover lexical formatting concerns that might overwise be handled by a formatter/pretty printer.
Use these rules when unable to use a formatter in your codebase. For example, perhaps a formatter does not conform to prescribed rules.

### AlignedParenthesis

Aligning Parentheses helps the reader visually parse the two tokens.

### Application.InArray.IndentArguments

This very limited rule ensures proper function call argument indentation when the function call is made inside an array.

While limited, this rule helps with the formatting of function calls in render functions.

### Application.InRecord.IndentArguments

This rule has a very limited scope: It ensures proper function call argument indentation when the function call is made while declaring a record literal.

### ArrayFormatting

Ensures consistent spacing when declaring an array literal.

### IfThenElse.LeftAligned

Aligning if/then/else tokens consistently helps readability. Left aligning is simply one aesthetic choice.

### LetBinding.VerticalCompact

This let formatting rule prioritizes conservation of vertical space.

### RecordFormatting

Ensures consistent spacing when declaring a record literal.

### WhereClause.LeftAligned

Consistent formatting of the where clause helps readability. Left aligning the where keyword with the bindings is simply one choice.

## Style (22)

Style rules cover code "correctness" or stylistic choices. For example, there is a rule to prefer the use of punning when available.

### Applicative.UnlessAndWhen

This rule asks that developers replace

- `when (not EXPR)` with `unless EXPR`
- `unless (not EXPR)` with `when EXPR`

In order to remove negation. Human beings read positive statements more easily.

### AvoidSameComparison

This rule forbids comparing two equivalent expressions because this will always return either true or false.

- `a == a`
- `a /= a`
- `a > a`
- `a >= a`
- etc..

### AvoidTypeAliases.OfOtherTypesDirectly

This is a very opinionated rule that forbids using a type alias for direct and simple aliasing of other types such as `type X = String` because they provide neither type safety nor code savings.

Instead of a type alias one might

- Author an actual `newtype` wrapping the other type. This provides safety and documentation.
- Place values of this kind into a record as a named field. This provides documentation and greatly reduces the chances a value will be used incorrectly in a context.

Type aliases of this kind are often used as a stepping stone to developing a full type and certainly have a place in the development process. Only apply this rule

- To areas of the codebase that require the highest quality and have the most re-use such as library code, Domain or Entity models, etc..
- When code is completely finished and ready to be merged. It might not be a good idea to fail the normal CI build for a feature branch as this would flag work in-progress. To do this author one config file with active development rules and another with final merge rules and run the CI pipeline accordingly.

### AvoidTypeAliases.WithAnonymousRecordsInContainerTypes

This is a VERY opinionated rule that forbids defining an alias that consists of some container **OF** an anonymous record ex. `type X = Array { name :: String, isCool :: Boolean }`.

Instead one might consider type aliasing the contained type. For example, in `type Points = Array { x :: Number, y :: Number }` the single point record should be aliased `type Point = { x :: Number, y :: Number }.`

### ModuleExports.RequireDocumentation

Require code documentation `-- |` of anything exported by a module to help developers understand usage. This can be configured with a regex pattern of types/type classes/values to exclude from this requirement.

### ModuleExports.Required

Requiring explicit exports ensures that developers are thinking about encapsulation and avoids missing opportunities to minimize the public surface area.

### ModuleImports.AvoidMultipleAliasesOfSameModule

Use this rule to ensure that a module is not imported under multiple aliases. The only exception is that one duplicate alias is allowed if it is used as a module re-export.

### ModuleImports.Forbid

Use this rule to ensure modules do not import other modules. This can be used as a cheap way to enforce decoupling between namespaces.

### ModuleImports.RequireQualification

Use this rule to ensure that developers

- Are not importing functions, values, and types with ambiguous names ex. `fromFoldable` is available for many container types such that a lack of qualification is confusing.
- Are qualifying modules consistently.

### Monoid.ReplaceIfThenElseMEmptyWithGuard

Replacing `if EXPR then TRUE else mempty` with `guard EXPR TRUE` reduces cognitive overhead because the reader should not be \"interested\" in the false branch.

This rule ONLY applies when the TRUE branch is considered a quickly executing expression. Expressions are considered quickly executing if they do not make function calls.
The reason expressions must be quickly executing is that the TRUE branch always evaluates when using `guard` and if that is slow you are better off using `if/then/else` which
delays execution until necessary.

This is similar to using `when EXPR TRUE` rather than `if EXPR then TRUE else pure unit` when working the applicatives.

### Monoid.ReplaceIfthenMEmptyElseWithGuard

Replacing `if EXPR then mempty else FALSE` with `guard (not EXPR) FALSE` _may_ reduce cognitive overhead. However, this case is a little more controversial.

This rule ONLY applies when the FALSE branch is considered a "quickly executing expression". Expressions are considered quickly executing if they do not make function calls.
The reason expressions must be quickly executing is that the expression always evaluates when using `guard` and if that is slow you are better off using `if/then/else` which
delays execution until necessary.

### Monoid.ReplaceMaybeMemptyWithFoldMap

Replacing `maybe mempty` with `foldMap` is a bit more succinct and more clearly expresses the intention.

### Monoid.ReplaceRepeatedMappendsWithFold

Using `fold`

1. Removes the need for parenthesis which reduces lexical noise.
2. Is more succinct, in terms of characters, when there are 8 or more mappends.

### NamingPatterns.Fields.OfContainerTypes

Enforces that names of record fields of "container" types follow a given naming pattern.

For example, some engineers like to prefix or suffix `Maybe` values to differentiate between values that must be unwrapped.

Lenses are often prefixed with an `_` when declared at a top level ex. `_Just`, `_Newtype` but you may also want to follow that convention if putting an optic into a record or row (using the A/An version to avoid issues of impredicativity).

### NoDuplicateTypeclassConstraints

The compiler does not complain about repeated type class constraints on a function, but it is unnecessary noise. This can happen during source control merges.

### NoUnnecessaryDo

A Monadic bind followed by a pure is actually a Functor map. It is more truthful to represent this as a narrower Functor map.

A `do` with no binds and no let declarations is unnecessary. Readers should expect a sequence of monadic binds when they see the `do` keyword.

### NoUnnecessaryParenthesis

Using parenthesis when unnecessary harms readability with unnecessary noise.

This rule identifies

- Unnecessary parenthesis in type declarations.
- SOME of the cases of unnecessary parenthesis in expressions. It is nowhere near as robust as hlint.

### UseAnonymous.BinaryOperations

It can be easier to read a wildcard operation than a lambda when the lambda body consists of a single binary operation.

With the default configuration this rule only applies to relational operations such as `<`, `<=`, `>`, `>=`, and `<>`.

### UseAnonymous.RecordCreation

It can be easier to read a wildcard record creation than to visually tie the arguments to the fields where they are used.

For example `\firstName lastName -> { firstName, middleInitial: "", lastName, suffix: "" }` could be re-written `{ firstName: _, middleInitial: "", lastName: _, suffix: "" }`.

### UseAnonymous.RecordUpdates

It is easier to read a wildcard record update than a lambda.

### UseForallSymbol

Some projects prefer using the ΓêÇ symbol rather than 'forall' to improve type signature readability and require less typing.

### UsePunning

Punning is easier to read because it reduces the noise of unnecessary repetition. By removing unnecessary repetition, true differences stand out more.

For example, can you spot the difference in `{ alice: alice, bob: bob, cindy: cindy', dave: dave }`?
Now with punning that is `{ alice, bob, cindy: cindy', dave }`.
