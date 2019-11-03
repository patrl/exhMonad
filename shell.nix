let
  jupyter = import (builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "10d64ee254050de69d0dc51c9c39fdadf1398c38";
  }) {};

  ihaskell = jupyter.kernels.iHaskellWith {
    name = "haskell";
    packages = p: with p; [ hvega formatting HaTeX ihaskell-hatex ];
  };

  ipython = jupyter.kernels.iPythonWith {
    name = "python";
    packages = p: with p; [ numpy ];
  };

  jupyterEnvironment = jupyter.jupyterlabWith {
    kernels = [ ihaskell ipython ];
  };
in
  jupyterEnvironment.env
