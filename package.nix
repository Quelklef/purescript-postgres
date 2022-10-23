let

pkgs = import ./pkgs.nix {};

npmlock2nix =
  import
    (pkgs.fetchFromGitHub
      { owner = "tweag";
        repo = "npmlock2nix";
        rev = "eeed152290ec2425f96c5e74e469c40b621e1468";
        sha256 = "sha256-HME6rnysvCwUVtH+BDWDGahmweMaLgD2wqHeRuGp6QI=";
      }
    ) { inherit pkgs; };

node_modules =
  "${npmlock2nix.node_modules { src = ./.; }}/node_modules";

in { ps-pkgs, ... }:
  with ps-pkgs;
  { dependencies =
      [ effect
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

    foreign."Database.Postgres.Connection".node_modules = node_modules;
    foreign."Database.Postgres.Query".node_modules = node_modules;
  }
