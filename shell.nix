{ pkgs ? import ./pkgs.nix {}
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
        rev = "66427405d2a3e0c2491646a6efc1716ce3731f3d";
        sha256 = "sha256-aArvsmkMc+LF2wEdLryiX/kqzMjtLsbcniIuSfFKgxg=";
      }
    ) { inherit system; };

package = import ./package.nix purs-nix;

nixed = purs-nix.purs
  { srcs = [ ./src ];
    inherit (package) dependencies foreign;

    test-dependencies =
      with purs-nix.ps-pkgs;
      [
        spec
      ];
  };

local-postgres =
  import
    (pkgs.fetchFromGitHub
      { owner = "quelklef";
        repo = "local-postgres";
        rev = "d3f377454632b10621af009313b3700424b369cf";
        sha256 = "sha256-67zbZjimGxYdx8rRpd0PBlMc7LMTUUNobDUpadfSlsA=";
      }) { inherit pkgs; };

in pkgs.mkShell {
  buildInputs =
    [ (nixed.command {
        srcs = [ "$PWD" ];
        test = "$PWD/test";
      })
      pkgs.nodejs
      pkgs.glibcLocales
      pkgs.postgresql
      local-postgres
    ];

    shellHook = ''

      function pg_init {
        pgloc=$PWD/pg
        [ -e $pgloc ] || lpg make $pgloc
        export PSPG_TESTING_DB_CONN_STRING=$(lpg bash $pgloc 'echo $LPG_CONNSTR')
      }

      function pg_start {
        lpg cmd ./pg pg_ctl start
      }

    '';
}
