{
  description = "A very basic flake";

  inputs = {
    roc.url = "github:roc-lang/roc";
  };

  outputs = { self, nixpkgs, roc }:
    let
      system = "x86_64-linux";
      overlay = (_: _: { roc-cli = roc.packages.${system}.default; });
      pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
    in {
      devShells.${system}.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs.buildPackages; [ roc-cli ];
      };
    };
}


#{
#  inputs = {
#    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
#    flake-utils.url = "github:numtide/flake-utils";
#    roc.url = "github:roc-lang/roc";
#  };
#
#  outputs = { self, nixpkgs, flake-utils, roc, ... }:
#    #flake-utils.lib.eachDefaultSystem (system:
#      
#    #);
#}