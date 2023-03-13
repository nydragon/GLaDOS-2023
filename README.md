# GLaDOS-2023

<p align="center">
    <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/3/39/Lambda_lc.svg/1200px-Lambda_lc.svg.png" alt="drawing" width="200">
    <p align="center">
        <img src="https://badgen.net/github/stars/nLatt/GLaDOS-2023?color=purple">
        <img src="https://badgen.net/github/contributors/nLatt/GLaDOS-2023?color=green">
        <img src="https://badgen.net/github/branches/nLatt/GLaDOS-2023?color=pink">
        <img src="https://badgen.net/github/commits/nLatt/GLaDOS-2023/main?color=orange">
    </p>
    <p align="center">
        <img src="https://github.com/nLatt/GLaDOS-2023/actions/workflows/haskell.yml/badge.svg?branch=main&event=push">
        <img src="https://github.com/nLatt/GLaDOS-2023/actions/workflows/haskell.yml/badge.svg?branch=develop&event=push">
    </p>
</p>

## Goal of the project

The goal of this project was two folds :

- First, to build a minimal [scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)) interpreter
- Second, build on this interpreter to make "our own" language

The repo currently reflects what came of the second part of the project. To look at our minimalistic scheme interpreter, you may take a look at the [corresponding release](https://github.com/nLatt/GLaDOS-2023/releases/tag/release).

For this **newly improved language,** we decided to attempt some sort of compilation. Unfortunately, dues to time constraints, we only had the time to implement compilation into **an assembly like language of our own design.** We then interpreted this language with another haskell program.

This solution still presented a couple advantages compared to interpretation :

- Runtime exceptions become **compilation erros**
- Optimization potential
- Type inference

## Installation

### Dependency Installation

Install cabal by installing GHCUP or another method described on the [Haskell website.](https://www.haskell.org/) Then run :

```sh
cabal update
```

Optionally, you may install the following vscode extensions :

```
hoovercj.haskell-linter
justusadam.language-haskell
haskell.haskell
```

### Building

For this project, we use a makefile in order to run various cabal commands.

You may build the project by doing :

```
make build <optional target>
```

When run with no optional target, the build command builds all binaries. You may also specify the build target, here are the options :

- glados
- runner
- sun-tzu

### Testing

The test command functions similarly to the build command.

```
make test <optional target>
```

By default, both unit and integration tests are run, however you may also specify one of the following :

- unit
- integration

## Features

- Written in **Haskell**
- **Abstract Syntax Tree** and **Concrete Parse Tree** implementation
- Standard input reading
- An [ABNF syntax description](./doc/syntax-description.md)
- An [ABNF syntax description for the "compiled" code](./doc/compiled-syntax-description)
- **Infix notations** for operators and custom functions

- **Strict** testing policy
    - Continuous integration running tests on every **Pull Request**
    - **Over 100** Unit tests and close to **50% coverage**
    - **Close to 20** Integration tests

### Supported Scheme Features

- Atoms
    - Signed Integers
    - Symbol
- Boolean type
- Function execution
- Function definition
    - Named Functions
    - Lambda Functions
- Variable definition
- Conditional expressions

### Builtin Functions

In addition to these basic features, our interpreter also implements the following builtins :

- println
- print
- div
- mod
- \*
- \-
- \+
- <
- <=
- \>
- \>=
- eq?

### Repl mode

You can also enter into an **interactive** session by running :

```
./glados -i
```

or

```
./glados --interactive
```
