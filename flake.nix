{
  description = "A very basic flake";

  inputs = {
    roc.url = "github:roc-lang/roc";
  };

  outputs = { self, nixpkgs, roc }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      rocPkgs = roc.packages.${system};
      rocFull = rocPkgs.full;
    in {
      formatter = pkgs.nixpkgs-fmt;
      devShells.${system} = {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [ rocFull ];

          shellHook = ''
            export ROC_LSP_PATH=${rocFull}/bin/roc_ls
          '';
        };
      };
    };
}
