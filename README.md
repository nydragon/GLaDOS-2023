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

# Features

- Basic **scheme interpreter**
- Written in **Haskell**
- **Strict** testing policy
    - Continuous integration running tests on every **Pull Request**
    - **Over 60** Unit tests and close to **50% coverage**
    - **Close to 20** Integration tests
- **Abstract Syntax Tree** and **Concrete Parse Tree** implementation
- Standard input reading
- An [ABNF syntax description](./doc/syntax-description.md)

## Supported Scheme Features

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

## Builtin Functions

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

## Repl mode

You can also enter into an **interactive** session by running :

```
./glados -i
```

or

```
./glados --interactive
```

## Install Dependencies

Install cabal by installing GHCUP or another method described on the [Haskell Website](https://www.haskell.org/).

```sh
cabal update;
```

Also install the following VSCode/Codium/CodeOSS extensions:
```
hoovercj.haskell-linter
justusadam.language-haskell
haskell.haskell
```

## Run project

```sh
cabal run glados -- file.scm;
```

or

```sh
make ARGS="file.scm"
```

## Running the tests

In order to see coloured output and no coverage when running the tests, please run the tests with one of the following commands:

```sh
cabal run test
```

```sh
make test
```

[Build Guide](doc/dev-install.md)

# Bonus ideas :

- Fully featured REPL
- Exception stack tracing
- Exception line tracing

