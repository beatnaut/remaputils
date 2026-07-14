let
  ## Define pinned nixpkgs version:
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/d073ef74d56480c6dd721d11e25159b7a75b2bf6.tar.gz";

  ## Import nixpkgs:
  pkgs = import nixpkgs { config = { }; overlays = [ ]; };

  ## Build missing R package `rdecaf`:
  rdecaf = pkgs.rPackages.buildRPackage {
    name = "rdecaf";
    src = pkgs.fetchFromGitHub {
      owner = "teloscube";
      repo = "rdecaf";
      rev = "72e5e9988efa0650137128bd2fcbb97d892c13fd";
      sha256 = "sha256-WKheFqCr6IcKFFnGYimLJNaTjuMCKxVeYToHyX6ZM4Q=";
    };
    propagatedBuildInputs = [
      pkgs.rPackages.httr
      pkgs.rPackages.jsonlite
      pkgs.rPackages.plyr
      pkgs.rPackages.readr
      pkgs.rPackages.crul
      pkgs.rPackages.R6
    ];
  };

  ## Declare development dependencies for R:
  devDependencies = [
    pkgs.rPackages.devtools
    pkgs.rPackages.testthat
    pkgs.rPackages.usethis
  ];

  ## Declare production dependencies for R:
  libDependencies = [
    pkgs.rPackages.jsonlite
    pkgs.rPackages.httr
    pkgs.rPackages.ghql
    pkgs.rPackages.digest
    pkgs.rPackages.RColorBrewer
    pkgs.rPackages.mailR
    pkgs.rPackages.stringdist
    pkgs.rPackages.tidyverse
    pkgs.rPackages.tseries
    pkgs.rPackages.tableHTML
    rdecaf
  ];

  ## Get R with development and production dependencies:
  thisR = pkgs.rWrapper.override {
    packages = devDependencies ++ libDependencies;
  };
in
pkgs.mkShell {
  buildInputs = [
    thisR
  ];
}
