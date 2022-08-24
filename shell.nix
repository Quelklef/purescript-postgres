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

nixed = purs-nix.purs
  { srcs = [ ./src ];
    dependencies =
      with purs-nix.ps-pkgs;
      [
        effect
        lists
        arrays
        maybe
        either
        aff
        aff-promise
        argonaut-core
        argonaut-codecs
        argonaut-generic
        spec  # actually a test dep, but purs-nix seems to be bugged (?)
      ];

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

in pkgs.mkShell {
  buildInputs =
    [ (nixed.command {
        srcs = [ "$PWD" ];
        test = "$PWD/test";
      })
      pkgs.nodejs
      pkgs.postgresql
    ];

    shellHook = ''

      root=$PWD

      function pspg.pg-init {
        mkdir -p "$root"/pg/{cluster,socket}
        initdb "$root"/pg/cluster
        pspg.pg-start
        createdb pspg
        createuser pspg
      }
      export PGDATA=$PWD/pg/cluster

      function pspg.pg-start {
        pg_ctl -l "$root"/pg/log -o "--unix_socket_directories='$root/pg/socket'" start
        # stop with pg_ctl stop
      }
      export PGHOST=$root/pg/socket

      function pspg.pg-obliterate {
        # Sometimes useful
        ps -aux | grep postgres | awk '{ print $2 }' | xargs sudo kill -9
      }

      alias pspg.psql="psql -d pspg -U pspg"

      export LC_ALL=C.UTF-8  # fix postgres
      export PSPG_TESTING_DB_CONN_STRING="postgresql://pspg@localhost?host=$root/pg/socket"

      function pspg.test {
        NODE_PATH=${node_modules}/node_modules purs-nix test
      }

      function pspg.test-watch {
        export -f pspg.test
        find src test | ${pkgs.entr}/bin/entr -cs pspg.test
      }

    '';
}
