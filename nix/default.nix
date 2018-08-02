let
  spec = builtins.fromJSON (builtins.readFile ./nixpkgs-src.json);
  rev = spec.rev;
  url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
in
  builtins.fetchTarball url
