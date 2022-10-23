{ pkgs ? import <nixpkgs> {}
, system ? builtins.currentSystem
}:

let

get-flake =
  import
    (pkgs.fetchFromGitHub
      { owner = "ursi";
        repo = "get-flake";
        rev = "703f15558daa56dfae19d1858bb3046afe68831a";
        sha256 = "1crp9fpvwg53ldkfxnh0wyxx470lm8bs025rck2bn5jn8jqmhj6f";
      });

purs-nix =
  get-flake
    (pkgs.fetchFromGitHub
      { owner = "ursi";
        repo = "purs-nix";
        rev = "18c1cae603b876c62515b7e4a3d4b587119e006b";
        sha256 = "0v78qgn0pdpmyy2wmyv0cig9mdflkcmaydgjqr6rxs4x3h1y4brv";
      }
    ) { inherit system; };

package = import ./package.nix purs-nix;

nixed = purs-nix.purs
  { srcs = [ ./src ];
    inherit (package) dependencies;

    test-dependencies =
      with purs-nix.ps-pkgs;
      [
        spec
      ];
  };

gitignoreSource =
  (import
    (pkgs.fetchFromGitHub
      { owner = "hercules-ci";
        repo = "gitignore.nix";
        rev = "a20de23b925fd8264fd7fad6454652e142fd7f73";
        sha256 = "07vg2i9va38zbld9abs9lzqblz193vc5wvqd6h7amkmwf66ljcgh";
      }
    ) { inherit (pkgs) lib; }).gitignoreSource;

npmlock2nix =
  import
    (pkgs.fetchFromGitHub
      { owner = "tweag";
        repo = "npmlock2nix";
        rev = "5c4f247688fc91d665df65f71c81e0726621aaa8";
        sha256 = "1zkrcph1vqgl0q7yi9cg0ghq1mmvhy8rlc6xvyww56i3j87cg5gn";
      }
    ) { inherit pkgs; };

node_modules = npmlock2nix.node_modules { src = gitignoreSource ./.; };

local-postgres =
  import
    (pkgs.fetchFromGitHub
      { owner = "quelklef";
        repo = "local-postgres";
        rev = "7a313efd50eb7710fee02b719728028486ff4526";
        sha256 = "1qw9b97f1pp0fyji0z684b0ja8b32n24m19izqj7km45sczqgljx";
      }) { inherit pkgs; };

in pkgs.mkShell {
  buildInputs =
    [ (nixed.command {
        srcs = [ "$PWD" ];
        test = "$PWD/test";
      })
      pkgs.nodejs
      pkgs.postgresql
      local-postgres
    ];

    shellHook = ''
      function init_pg {
        pgloc=$PWD/pg
        [ -e $pgloc ] || lpg make $pgloc
        export PSPG_TESTING_DB_CONN_STRING=$(lpg do $pgloc bash -c 'echo $LPG_CONNSTR')
      }
    '';
}
