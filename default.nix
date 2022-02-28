{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, brick, fast-logger, lib
      , network, optparse-generic, random, text, vty, websockets, witch
      }:
      mkDerivation {
        pname = "FreeSnaky";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson async base brick fast-logger network random text vty
          websockets witch
        ];
        executableHaskellDepends = [
          async base fast-logger optparse-generic
        ];
        homepage = "https://github.com/morucci/FreeSnaky#README.md";
        description = "Free Snaky";
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
