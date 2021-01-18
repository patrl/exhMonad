let
  haskellCompiler = "ghc8102";
  sources = import ./nix/sources.nix;
  haskellNix = import sources.iohk-hnix {};
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  pkgs = import nixpkgsSrc nixpkgsArgs;
  hspkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "exhMonad";
      src = ./.;
    };
    compiler-nix-name = "ghc8102";
  };
  shell = hspkgs.shellFor {
    withHoogle = true;
    tools = {
      cabal = "latest";
      haskell-language-server = "latest";
      brittany = "latest";
      hlint = "latest";
      ghcid = "latest";
      hpack = "latest";
      ormolu = "latest";
    };
    exactDepts = true;
  };
in { 
  inherit shell;
  inherit hspkgs;
  exhMonad = hspkgs.exhMonad;
}
