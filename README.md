# FreeSnaky

## Build

You need to have ghc and cabal installed. See instruction for your distribution. Or use
nix to get a shell with ghc and cabal:

```
nix-shell -p ghc -p cabal-install
```

Then optionally spawn the FreeSnaky nix-shell to get Shell required Hashell dependencies
to build the project.

```
nix-shell
```

Then Simply run:

```
cabal build
```

## Start the Server

```
cabal repl
λ> runLocalServer
```

## Start the Terminal Client

```
cabal repl
λ> runLocalClient
```
