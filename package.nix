{ ps-pkgs, ... }:
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
  }
