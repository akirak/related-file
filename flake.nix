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

        default = related-file;
      };

      checks.javascript = pkgs.runCommandLocal "check" {
        src = ./testdata;
        buildInputs = [
          self.packages.${system}.related-file
        ];
      } ''
        tmp=$(mktemp)
        related-file $src/javascript/hello.js > $tmp
        [[ $(grep ^test $tmp) = "test $src/javascript/__tests__/hello.spec.js" ]]
        [[ $(grep ^stories $tmp) = "stories $src/javascript/__stories__/hello.stories.js" ]]

        related-file $src/javascript/__tests__/hello.spec.js > $tmp
        [[ $(grep ^impl $tmp) = "impl $src/javascript/hello.js" ]]

        related-file $src/javascript/__stories__/hello.stories.js > $tmp
        [[ $(grep ^impl $tmp) = "impl $src/javascript/hello.js" ]]

        touch $out
      '';

      checks.emacs-lisp-1 = pkgs.runCommandLocal "check" {
        src = ./testdata;
        buildInputs = [
          self.packages.${system}.related-file
        ];
      } ''
        tmp=$(mktemp)

        related-file $src/emacs-lisp-1/hello.el > $tmp
        [[ $(grep ^test $tmp) = "test $src/emacs-lisp-1/hello-test.el" ]]

        related-file $src/emacs-lisp-1/hello-test.el > $tmp
        [[ $(grep ^impl $tmp) = "impl $src/emacs-lisp-1/hello.el" ]]

        touch $out
      '';

      checks.elixir = pkgs.runCommandLocal "check" {
        src = ./testdata;
        buildInputs = [
          self.packages.${system}.related-file
        ];
      } ''
        tmp=$(mktemp)

        related-file $src/elixir/lib/hello_web/controllers/page_controller.ex > $tmp
        [[ $(grep ^test $tmp) = "test $src/elixir/test/hello_web/controllers/page_controller_test.exs" ]]

        related-file $src/elixir/test/hello_web/controllers/page_controller_test.exs > $tmp
        [[ $(grep ^lib $tmp) = "lib $src/elixir/lib/hello_web/controllers/page_controller.ex" ]]

        touch $out
      '';

      devShells.default = pkgs.mkShell {
        buildInputs = [
          pkgs.ciao
        ];
      };
    });
}
