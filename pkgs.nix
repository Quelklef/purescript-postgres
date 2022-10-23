args: let

pkgs =
  let
    rev = "37cc765b36b0f74e4f18800e466496bacb366a35";
    src =
      builtins.fetchTarball
        "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  in
    import src args;

in pkgs
