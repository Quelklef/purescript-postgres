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
      let ns = purs-nix.ps-pkgs-ns; in
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
      ];
  };

npmlock2nix =
  let fetched = pkgs.fetchFromGitHub {
        owner = "tweag";
        repo = "npmlock2nix";
        rev = "8ada8945e05b215f3fffbd10111f266ea70bb502";
        sha256 = "0ni3z64wf1cha7xf5vqzqfqs73qc938zvnnbn147li1m4v8pnzzx";
      };
  in import fetched { inherit pkgs; };

node_modules = npmlock2nix.node_modules { src = ./.; };

in pkgs.mkShell {
  buildInputs =
    [ (nixed.command { srcs = [ "$PWD" ]; })
      pkgs.nodejs
      pkgs.postgresql
    ];

    shellHook = ''

      local root=$PWD

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

      function y.pg-obliterate {
        # Sometimes useful
        ps -aux | grep postgres | awk '{ print $2 }' | xargs sudo kill -9
      }

      export LC_ALL=C.UTF-8  # fix postgres
      export PSPG_DB_CONN_STRING="postgresql://pspg@localhost?host=$root/pg/socket"

    '';
}
