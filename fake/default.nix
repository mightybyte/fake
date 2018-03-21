# To pin to a specific version of nixpkgs, you can substitute <nixpkgs> with:
# `(builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/<nixpkgs_commit_hash>.tar.gz")`
{ pkgs ? import <nixpkgs> {} }: pkgs.haskellPackages.developPackage
  { root = ./.;
    overrides = self: super:
      { # Don't run a package's test suite
        # foo = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.foo;
        #
        # Don't enforce package's version constraints
        # bar = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.bar;
        #
        # To discover more functions that can be used to modify haskell
        # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
        # <TAB> to get a tab-completed list of functions.
      };
    source-overrides =
      { # Use a specific hackage version
        # io-streams = "1.4.0.0";
        #
        # Use a particular commit from github
        # text = pkgs.fetchFromGitHub
        #   { owner = "haskell";
        #     repo = "text";
        #     rev = "ab90c65cdb7cd5bcbd739843b98bb5da515c8bce";
        #     sha256 = "0phar79fky4yzv4hq28py18i4iw779gp5n327xx76mrj7yj87id3";
        #   };
      };
  }
