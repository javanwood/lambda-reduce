{
  description = "elm flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... } @ inputs:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        localNixBin = "./bin/nix-links"; # Referenced in vscode settings, .gitignore
        vscodeBinaries = ''
          mkdir -p ${localNixBin}
          echo "Nix managed binaries for certain tools that need to refer to a path." > ${localNixBin}/readme.txt
          ln -sf "$(which elm-review)" ${localNixBin}/elm-review
          ln -sf "$(which elm-format)" ${localNixBin}/elm-format
          ln -sf "$(which elm-test)" ${localNixBin}/elm-test
          ln -sf "$(which elm)" ${localNixBin}/elm
        '';

        fetchElmDependencies = pkgs.elmPackages.fetchElmDeps {
          elmPackages = import ./nix/elm/elm-srcs.nix;
          registryDat = ./nix/elm/registry.dat;
          elmVersion = "0.19.1";
        };
      in
        {
          legacyPackages = pkgs;

          devShell = pkgs.mkShell {
            buildInputs = [
              pkgs.elmPackages.elm
              pkgs.elmPackages.elm-format
              pkgs.elmPackages.elm-language-server
              pkgs.elmPackages.elm-review
              pkgs.elmPackages.elm-test
              pkgs.elmPackages.elm-json
            ];
            shellHook = fetchElmDependencies + vscodeBinaries;
          };
      }
    ));
}
