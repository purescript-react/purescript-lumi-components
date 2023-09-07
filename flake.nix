{
  description = "Provide an environment for working in this repo";

  # to handle mac and linux
  inputs.flake-utils.url = "github:numtide/flake-utils";

  # we want to use a consistent nixpkgs across developers.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = all@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        packages =
          let
            # everything we want available in our development environment that isn't managed by
            # npm, spago
            # we do not differentiate between libraries needed for building and tools at the moment.
            sharedPackages = with pkgs; [
              nodejs-16_x
            ];
          in
            sharedPackages;
      in {
        # produce our actual shell
        devShell = pkgs.mkShell rec {
          # make our packages available
          buildInputs = packages;
        };
      }
    );
}
