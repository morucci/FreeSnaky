{
  description = "FreeSnaky";
  nixConfig.bash-prompt = "[nix(FreeSnaky)] ";

  inputs = {
    hspkgs.url = "github:podenv/hspkgs/24d2028871584f71313ac06e23ef143db61aea34";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, hspkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        packageName = "FreeSnaky";
        pkgs = hspkgs.pkgs;
        haskellPackages = pkgs.hspkgs;
        myPackage = haskellPackages.callCabal2nix packageName self { };

      in {
        defaultExe = pkgs.haskell.lib.justStaticExecutables myPackage;
        defaultPackage = myPackage;

        devShell = haskellPackages.shellFor {
          packages = p: [ myPackage ];

          buildInputs = [
            pkgs.ghcid
            pkgs.ormolu
            pkgs.cabal-install
            pkgs.hlint
            pkgs.haskell-language-server
          ];
        };
      });
}
