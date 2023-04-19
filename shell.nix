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
      pkgs.glibcLocales
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
