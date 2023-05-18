{
  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem
    (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
    in {
      packages = flake-utils.lib.flattenTree rec {
        related-file = pkgs.runCommand "related-file" {
          buildInputs = [
            pkgs.ciao
          ];
        } ''
          install -d $out/bin
          ciaoc -s -o $out/bin/related-file ${./related-file.pl}
        '';

        default = self.packages.${system}.related-file;
      };

      devShells.default = pkgs.mkShell {
        buildInputs = [
          pkgs.ciao
        ];
      };
    });
}
