{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        config = { };
        overlays = builtins.attrValues self.overlays;
      });
    in {
      overlays = {
        purescript = inputs.purescript-overlay.overlays.default;
      };

      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.hello; # your package here
        });

      devShells = forAllSystems (system:
        # pkgs now has access to the standard PureScript toolchain
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.mkShell {
            name = "my-purescript-project";
            inputsFrom = builtins.attrValues self.packages.${system};
            buildInputs = with pkgs; [
              purs
              spago-unstable
              purs-tidy-bin.purs-tidy-0_10_0
              purs-backend-es
              # nodejs-16_x
              nodejs-18_x
              # nodejs_latest
            ];

            shellHook = ''
              source <(spago --bash-completion-script `which spago`)
              source <(node --completion-bash)
            '';
          };
        });
  };
}
