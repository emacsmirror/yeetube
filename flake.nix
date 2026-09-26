{
  description = "YouTube front-end for Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  inputs.nixpkgs-emacs291.url = "github:NixOS/nixpkgs/057f9aecfb71c4437d2b27d3323df7f93c010b7e";

  outputs = { self, nixpkgs, nixpkgs-emacs291 }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkYeetube = system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          emacs = pkgs.emacs;
          minimum = (import nixpkgs-emacs291 { inherit system; }).emacs29-nox;
          emacsPackages = pkgs.emacsPackagesFor emacs;

          # Makefile owns the source/test manifest, including reload order.
          manifest = name:
            lib.splitString " " (builtins.head
              (builtins.match ".*\n${name} = ([^\n]+)\n.*"
                (builtins.readFile ./Makefile)));
          ordinaryTests = map (name: "test/${name}")
            (lib.filter (name: lib.hasSuffix "-tests.el" name)
              (builtins.attrNames (builtins.readDir ./test)));
          source = assert lib.assertMsg
            (lib.sort builtins.lessThan ordinaryTests == lib.sort builtins.lessThan (manifest "TESTS"))
            "TESTS must list every ordinary test suite";
            lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions ([ ./Makefile ] ++
              map (name: ./. + "/${name}")
                (manifest "SRCS" ++ manifest "TESTS" ++ manifest "FIXTURES" ++ manifest "MATRIX_FILES"));
          };

          keymapPopupVersion = "0.4.1";

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = pkgs.fetchurl {
              url = "https://elpa.gnu.org/packages/keymap-popup-${keymapPopupVersion}.tar.lz";
              hash = "sha256-3Xs51u3K6LT3xRqRn82FrGfoKg9oXBRZSojpFAMmjXA=";
            };
            nativeBuildInputs = [ pkgs.lzip ];
            packageRequires = [ ];
          };

          emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
            keymapPopup
            epkgs.compat
          ]);

          testDependencies = pkgs.runCommand "yeetube-test-dependencies" { } ''
            mkdir -p $out
            find -L ${keymapPopup} ${emacsPackages.compat} -name '*.el' -type f \
              -exec cp '{}' $out/ \;
          '';
          matrixTools = [ pkgs.python3 pkgs.gnumake ];
          matrixShell = runtime: pkgs.mkShellNoCC {
            packages = matrixTools ++ [ runtime ];
            YEETUBE_MATRIX_DEPS = testDependencies;
            YEETUBE_MATRIX_VERSION = runtime.version;
          };
          matrixCheck = name: runtime: pkgs.stdenvNoCC.mkDerivation {
            pname = "yeetube-matrix-${name}";
            version = "git";
            src = source;
            nativeBuildInputs = matrixTools ++ [ runtime ];
            YEETUBE_MATRIX_DEPS = testDependencies;
            dontConfigure = true;
            buildPhase = ''
              python3 admin/test-matrix --lane ${name} --expected ${runtime.version} \
                --root "$TMPDIR/lane"
            '';
            installPhase = ''
              mkdir -p $out
              cp "$TMPDIR/lane/"*.json "$TMPDIR/lane/passed" $out/
            '';
          };

          tests = pkgs.stdenv.mkDerivation {
            pname = "yeetube-tests";
            version = "git";
            src = source;
            nativeBuildInputs = [
              emacsWithPackages
              pkgs.gnumake
            ];
            dontConfigure = true;

            buildPhase = ''
              runHook preBuild
              unset EMACSDATA EMACSDOC EMACSLOADPATH EMACSPATH GREP_OPTIONS
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/share"
              export XDG_STATE_HOME="$TMPDIR/state"
              mkdir -p "$HOME" "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" \
                "$XDG_DATA_HOME" "$XDG_STATE_HOME"
              EMACS_CMD=emacs \
                YEETUBE_ENV_WRAPPED=1 \
                make dev
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              touch $out/tests-passed
              runHook postInstall
            '';
          };
        in {
          inherit emacs emacsWithPackages keymapPopup pkgs tests minimum matrixShell matrixCheck source;
        };
    in {
      checks = forAllSystems (system:
        let yeetube = mkYeetube system;
        in {
          test = yeetube.tests;
          matrix-minimum = yeetube.matrixCheck "minimum" yeetube.minimum;
          matrix-default = yeetube.matrixCheck "default" yeetube.emacs;
          matrix-runner = yeetube.pkgs.runCommand "yeetube-matrix-runner" {
            nativeBuildInputs = [ yeetube.pkgs.python3 ];
          } ''
            python3 ${yeetube.source}/admin/test-matrix-runner.py
            touch $out
          '';
        });

      devShells = forAllSystems (system:
        let yeetube = mkYeetube system;
        in {
          matrix-minimum = yeetube.matrixShell yeetube.minimum;
          matrix-default = yeetube.matrixShell yeetube.emacs;
          default = yeetube.pkgs.mkShell {
            packages = with yeetube.pkgs; [
              cacert
              yeetube.emacsWithPackages
              git
              gnumake
            ];

            shellHook = ''
              export EMACS_CMD=emacs
            '';
          };
        });
    };
}
