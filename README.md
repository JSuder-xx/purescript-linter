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

This creates a default configuration file which determines which rules apply to specified filepaths. The default assumes your source code lives under a `src` path. If that is not the case then please edit the `purs-lint.json` file and update

- Both the `pathPrefix` of `projectRoots`
- And also update the `globs` property of the initialized rule set.

The linter can then be run from the root of your project with

```
npx purescript-linter
```

The linter does not _currently_ support any direct IDE integration but the linter can be run for a single file with the `-f` command line flag.

## Rules Overview

There are 28 total [Rules](Rules.md) in two categories

- Formatting (8 total) Rules assert how code should be formatted. This can be useful if you cannot, for company policy reasons, or do not wish to use a code formatter such as the excellent `purs-tidy`.
- Style (20 total) Rules enforce coding style such as requiring code documentation for exports, asking developers to use record field punning when available, etc..

## Configuration

Configuration of the linter is, by default, placed into a file at the root of your project named `purs-lint.json`. The `purs-lint.json` file has a JSON Schema with helpful descriptions that will be published for integration with VS Code.

## Alternate or Multiple Config Files

Tell the linter to use an alternate config file by using the `-c` command line argument. Multiple config files can be useful for different use cases. For example

- One config might house the rules that run in CI and fail the build. Those rules are likely to be fewer and more lenient.
- An alternate set of more extensive or stricter rules could be used as part of a local development workflow.

## Rule Sets

The fundamental unit of specification is the Rule Set. A rule set consists of an array of `globs` that are used to determine the target files of the rule set as well as a `rules` record where the keys are rule names and the values individual rule configuration. A single configuration file can have many Rule Sets. For example

- A broadly targeted rule set might have `globs: [ "./src/**/*.purs" ]` with rules that should apply to the entire codebase.
- While a more narrowly targeted rule set might target just the entity models `globs: [ "./src/Entity/**/*.purs" ]` with much stricter rules.

## Contribution

Please feel free to make a pull request for bug fixes, enhancements, improved documentation, and, especially, new rules.

## Motivation and Alternatives

Work on this linter began before I was aware of any real [Alternatives](Alternatives.md). Today there are a few other projects with a similar concern that you can find on Github and I would rather people made an informed choice of which project they would like to apply to their codebase.

## Thanks

This linter is possible thanks to the excellent work of Nathan Faubion on [purescript-language-cst-parser](https://github.com/natefaubion/purescript-language-cst-parser).
