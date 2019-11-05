let
  jupyter = import (builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "";
  }) {};

  ihaskellWithPackages = jupyter.kernels.iHaskellWith {
      name = "hExh";
      packages = p: with p; [
        (p.callPackage ./hExh {})
        HaTeX
        ihaskell-hatex
      ];
    };

  jupyterlabWithKernels =
    jupyter.jupyterlabWith {
      kernels = [ ihaskellWithPackages ];
      directory = jupyter.mkDirectoryWith {
        extensions = [
          "jupyterlab-ihaskell"
        ];
      };
    };
in
  jupyterlabWithKernels.env
