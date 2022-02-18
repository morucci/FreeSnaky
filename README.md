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

## Start a local party

```
cabal run FreeSnaky -- local
```

## Client / Server mode
### Start the Server

```
cabal run FreeSnaky -- server --bindAddress 127.0.0.1
```

### Start the Terminal Client

```
cabal run FreeSnaky -- client --address 127.0.0.1
```
