# Overview

Below you will find two linter alternatives of which I am aware. I document what I know about those other projects and how those projects may different from PureScript Linter so that you can make an informed decision about which linter is the best choice for your codebase.

## purescript-cst-linter

[purescript-cst-linter](https://github.com/avi892nash/purescript-cst-linter) is a linter with 4 rules (at the time of this writing) and it looks like it either already has IDE integration or that work is in progress. This linter seems to have been started about the same time as PureScript Linter. The first commit of `purescript-cst-linter` was July 6 2024 while the first commit of `purescript-linter` was on July 10 2024. I probably completed my due diligence sometime around July 3 thus missing knowledge of this product by a few days!

## purescript-whine

[purescript-whine](https://github.com/collegevine/purescript-whine) a linter with 5 rules (at the time of this writing), VS Code integration, and the ability to comment code with "escape hatches" to disable rule application. The author had also stated, at some point, a goal of making linter rules pluggable / loading from external files. The codebase looks elegant and well organized. You may prefer Whine to this Linter but I decided to continue building PureScript Linter for the following reasons

- Different philosophical positions on a few design elements.
  - "Escape hatches" to disable rule application to specific code inside of a module. While offered by other popular linters such as `ESLint`, I believe that disabling rules using comments inside of a module harms readability with noise, makes it "too easy" to disable rules thus reducing the normalizing power of rules, and it complicates the tooling. PureScript Linter _does_ provide the ability to custom tailor which modules rules will apply to with the Rule Sets. I would rather developers think about how to engineer their code to conform to the coding standard or decide that an entire module is privileged or unusual and so should be excluded from linting altogether.
  - The author may have changed their mind but the goal of making rules pluggable also sounded warning bells as plug-in systems are likely to lead to run-time bloat / performance problems. I could be wrong about the intent and I could be wrong about the performance.
- Inertia: I had already written over 20 rules for this Linter and the value proposition was not favorable to convert the rules to a different system. My goal with the Linter is very practically motivated (not at all academic) - I want to spend less time giving and receiving trivial guidance in code reviews so that my entire team (9 engineers) can focus on real topics such as data modeling, decoupling, readability, abstraction, re-use, etc.. As such, I am more interested in writing new rules that advance that goal than anything else and I already had a battle tested linter tool of my own in use and adding value.
