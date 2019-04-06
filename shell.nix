let hie = import(builtins.fetchGit {
      name = "hie-nix";
      url = "https://github.com/domenkozar/hie-nix.git";
      ref = "master";
      rev = "6794005f909600679d0b7894d0e7140985920775";
    }) {};
    nixpkgs = import(builtins.fetchGit {
      name = "hixpkgs";
      url = "https://github.com/NixOS/nixpkgs.git";
      ref = "master";
      rev = "2e501366033740c3ad1c5ca298d88add13a9f8a6";
      }) {};
in
nixpkgs.stdenv.mkDerivation {
  name = "hie-env";
  buildInputs =
    [ nixpkgs.stack nixpkgs.zlib.dev nixpkgs.zlib.out hie.hies
      nixpkgs.cabal-install
    ];
}
