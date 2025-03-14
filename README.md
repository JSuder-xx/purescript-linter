# purescript-linter

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

There are 28 total [Rules](Rules.md) in two categories

- Formatting (8 total) Rules assert how code should be formatted. This can be useful if you cannot, for company policy reasons, or do not wish to use a code formatter such as the excellent `purs-tidy`.
- Style (20 total) Rules enforce coding style such as requiring code documentation for exports, asking developers to use record field punning when available, etc..

See the repository for more on the rules.
