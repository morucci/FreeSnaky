{
  description = "FreeSnaky";
  nixConfig.bash-prompt = "[nix(FreeSnaky)] ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          # config.allowBroken = true;
        };
        packageName = "FreeSnaky";
        haskellPackages = pkgs.haskellPackages;
        myPackage = haskellPackages.callCabal2nix packageName self { };

      in {
        defaultExe = pkgs.haskell.lib.justStaticExecutables myPackage;
        defaultPackage = myPackage;

        devShell = haskellPackages.shellFor {
          packages = p: [ myPackage ];

          buildInputs = with haskellPackages; [
            ghcid
            ormolu
            cabal-install
            hlint
            pkgs.haskell-language-server
          ];
        };
      });
}
