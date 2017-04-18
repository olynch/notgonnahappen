{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, esqueleto, monad-logger, persistent
      , persistent-postgresql, persistent-template, protolude
      , shakespeare, stdenv, template-haskell, yesod, yesod-auth
      , yesod-core, yesod-static
      }:
      mkDerivation {
        pname = "notgonnahappen";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base esqueleto monad-logger persistent persistent-postgresql
          persistent-template protolude shakespeare template-haskell yesod
          yesod-auth yesod-core yesod-static
        ];
        homepage = "notgonnahappen.me";
        description = "A logging metronome - if you don't practice every day it's not gonna happen";
        license = stdenv.lib.licenses.agpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
