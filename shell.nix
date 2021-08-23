{ pkgs ? import <nixpkgs> {} }:

let

purs-nix =
  import
    (builtins.fetchGit
       { url = "https://github.com/ursi/purs-nix.git";
         rev = "988505248316b1dc82504c8e25358da535e34bd6";
       }
    ) {};

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
  let src = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "211907489e9f198594c0eb0ca9256a1949c9d412";
    sha256 = "06j7wpvj54khw0z10fjyi31kpafkr6hi1k0di13k1xp8kywvfyx8";
  };
  in (import src { inherit (pkgs) lib; }).gitignoreSource;

npmlock2nix =
  let fetched = pkgs.fetchFromGitHub {
        owner = "tweag";
        repo = "npmlock2nix";
        rev = "8ada8945e05b215f3fffbd10111f266ea70bb502";
        sha256 = "0ni3z64wf1cha7xf5vqzqfqs73qc938zvnnbn147li1m4v8pnzzx";
      };
  in import fetched { inherit pkgs; };

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
