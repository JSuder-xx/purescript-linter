### 1.1.0 (2025-03-10)

- Added rule `ModuleExports.RequireDocumentation`
- Enhanced the README.md a bit.

### 1.0.0 (2025-03-06)

- Added verbosity level to reporting.
- Added rule `Applicative.UnlessAndWhen`
- Added rule `AvoidSameComparison`
- Chose less ambiguous filename for config, added JSON Schema for `purs-lint.json`, and normalized rule names (BREAKING CHANGE).
- Fixed: `WhereClause.LeftAligned` was failing to recurse.
- Changed: Running Quiet on Single File linting.
- Added configuration for `Monoid.ReplaceRepeatedMappendsWithFold`
- Added rule `ModuleImports.Forbid`
- Switching to semantic versioning. All versions from here will follow major breaking, minor enhancement/addition, and release for bug fixes.

### 0.2.14 (2025-02-21)

- Fixed: Monoid simplification rules recommending `guard` applied to situations where `guard` could have been slow and therefore not desirable.
- Changed: Made the `UseAnonymous-ForOperations` configurable and defaulted to only applying relational operators.

### 0.2.13 (2025-02-20)

- Added: Checking for unnecessary parenthesis in type declarations.
- Fixed: Imprecise error messages for delimited formatting.

### 0.2.12 (2025-02-018)

- Fixed: Single file reporting

### 0.2.11 (2025-02-17)

- Changed: Friendlier error messages for config decode error when a rule name is not found.
- Changed: Improved speed for multiple rule sets with overlapping files by combining in first pass and then loading/parsing each file only once on second pass.
- Changed: Removed current working directory prefix from emited files errors.
- Changed: Default initial config to "recommended" rules which excludes formatting and controversial rules.

### 0.2.9 (2025-02-14)

- Expanded application of NoDuplicateTypeclassConstraints to all types (and not just declarations).
- Added: UseForallSymbol rule.
- Added: Hardcoded rule to verify that Module Names match their File Path.
- Added: Display of time it took to run each Rule Set.
- Added: ModuleImportQualification rule.

### v0.2.8 (2024-10-27)

- Fixed bug in NoDuplicateTypeclassConstraints.

### v0.2.7 (2024-8-26)

- Added: Command line argument to show all rule names and descriptions.
- Added: Some limited function application formatting rules.

### v0.2.6 (2024-8-20)

- Command line arguments

### v0.2.5 (2024-8-16)

- Added: Monoid Simplification Rules

### v0.2.4 (2024-8-14)

- Bug Fix: The IfThenElseLeftAligned rule did not handle chains of if/then/else.

### v0.2.3 (2024-8-13)

- Bug Fix: Expressions did not recurse into Application terms.

### v0.2.2 (2024-8-8)

- Bug fixes

### v0.2.1 (2024-8-8)

- Forgot to rebuild dist 🤦‍♂️

### v0.2.0 (2024-8-8)

- Refactored Linter to Rule.
- Setting process Exit Code so that CI fails appropriately.

### v0.1.0 (2024-8-6)

- Initial release
