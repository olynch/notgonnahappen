{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, case-insensitive
      , classy-prelude, classy-prelude-conduit, classy-prelude-yesod
      , conduit, containers, data-default, directory, fast-logger
      , file-embed, hjsmin, hspec, http-conduit, monad-control
      , monad-logger, persistent, persistent-postgresql
      , persistent-template, resourcet, safe, shakespeare, stdenv
      , template-haskell, text, time, transformers, unordered-containers
      , vector, wai, wai-extra, wai-logger, warp, yaml, yesod, yesod-auth
      , yesod-core, yesod-form, yesod-static, yesod-test
      }:
      mkDerivation {
        pname = "notgonnahappen";
        version = "0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring case-insensitive classy-prelude
          classy-prelude-conduit classy-prelude-yesod conduit containers
          data-default directory fast-logger file-embed hjsmin http-conduit
          monad-control monad-logger persistent persistent-postgresql
          persistent-template safe shakespeare template-haskell text time
          unordered-containers vector wai wai-extra wai-logger warp yaml
          yesod yesod-auth yesod-core yesod-form yesod-static
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [
          aeson base classy-prelude classy-prelude-yesod hspec monad-logger
          persistent persistent-postgresql resourcet shakespeare transformers
          yesod yesod-auth yesod-core yesod-test
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
