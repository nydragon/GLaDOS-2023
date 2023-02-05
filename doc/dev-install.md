# Install Dependencies


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

# Run project

```sh
cabal run glados -- file.scm;
```

or

```sh
make ARGS="file.scm"
```

# Running the tests

In order to see coloured output and no coverage when running the tests, please run the tests with one of the following commands:

```sh
cabal run test
```

```sh
make test
```