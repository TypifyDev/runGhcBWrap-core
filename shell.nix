{ nixpkgs ? import <nixpkgs> {} }:
let
  pkgs = nixpkgs;

  pkgs_unstable = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
  }) {};

  IStr-src = pkgs.fetchFromGitHub {
    owner = "augyg";
    repo = "IStr";
    rev = "7dfe4a59ee0f684024d79ba6b4141a06bc70f616";
    sha256 = "1hwrnn67nb7v3nag5anknhp7ayc0ps9k9lnl2lc053vw0554iw65";
  };

  scrappy-core-src = pkgs.fetchFromGitHub {
    owner = "TypifyDev";
    repo = "scrappy-core";
    rev = "9dba0faa28b9b5fb1f354eddf573e6b78b0fb751";
    sha256 = "09hv84d0dfkmwi6sgpcx617vhg874qv6bn89xl8z4kz046v7dhcy";
  };

  overrides = self: super: {
    IStr = self.callCabal2nix "IStr" IStr-src {};
    scrappy-core = self.callCabal2nix "scrappy-core" scrappy-core-src {};
    runGhcBWrap-core = pkgs_unstable.haskell.lib.doJailbreak
      (self.callCabal2nix "runGhcBWrap-core" ./. {});
  };

  haskellPackages = pkgs_unstable.haskell.packages.ghc912.override {
    inherit overrides;
  };

  drv = haskellPackages.runGhcBWrap-core;
  shell = drv.env.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
      pkgs_unstable.cabal-install
    ];
  });
in
  if pkgs.lib.inNixShell then shell else drv
