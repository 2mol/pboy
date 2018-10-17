let pkgs = import (import ./nix) {}; in
pkgs.callPackage ./pboy_dynamic.nix {}
